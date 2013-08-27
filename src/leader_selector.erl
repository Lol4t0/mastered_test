-module(leader_selector).
-behaviour(gen_server).

-export([start/5, start_link/5, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([behaviour_info/1]).
behaviour_info(callbacks) ->
    [{init,1}, {restart_leader,1}, {restart_idle,2}, {address,1}, {terminate,1}].


log(Msg, Args) when is_list(Args) ->
	io:format(lsts:concat("MASTERED_TEST::", Msg, "~n"), Args);
log(msg, arg) ->
	log(msg, [arg]).

%% The startfunction.
start(ConnectionPId, WorkerModule, WorkerParams, Prefix, NodeNum) ->
    gen_server:start(?MODULE, [ConnectionPId, WorkerModule, WorkerParams, Prefix, NodeNum], []).

start_link(ConnectionPId, WorkerModule, WorkerParams, Prefix, NodeNum) ->
    gen_server:start_link(?MODULE, [ConnectionPId, WorkerModule, WorkerParams, Prefix, NodeNum], []).


init([ConnectionPId, WorkerModule, WorkerParams, Prefix, NodeNum]) ->
	SelfNodeName = io_lib:format("~s/~w", [Prefix, NodeNum]),
	log("Prefix: ~s", Prefix),
	{ok, _Path} = ezk:create(ConnectionPId, SelfNodeName, WorkerModule:address(WorkerParams), e), % e for EPHEMERAL
	WorkerState = WorkerModule:init(WorkerParams),
	reelect({_SelfState = {ConnectionPId, WorkerModule, Prefix, NodeNum}, WorkerState}).

reelect(FullState = {{ConnectionPId, _WorkerModule, Prefix, NodeNum}, _WorkerState}) ->
	{ok, List} = ezk:ls(ConnectionPId, Prefix, self(), nodes_changed),
	ActiveNodes = lists:sort(lists:map (fun(<<Path>>) -> list_to_integer(Path) end, List)),
	log("active nodes: ~w", ActiveNodes),
	_Return = case lists:takeWhile(fun(N) -> N < NodeNum end) of
		[] ->
			log("starting as leader", []),
			restart_as_leader(FullState);
		Nodes ->
			ActiveNode = lists:last(Nodes),
			log("starting as idle with active node: ~w", ActiveNode),
			restart_as_idle(FullState, ActiveNode)
	end.

restart_as_leader({SelfState = {_ConnectionPId, WorkerModule, _Prefix, _NodeNum}, WorkerState}) ->
	NewWorkerState = WorkerModule:restart_leader(WorkerState),
	{SelfState, NewWorkerState, leader}.

restart_as_idle({SelfState = {ConnectionPId, WorkerModule, Prefix, _NodeNum}, WorkerState}, ActiveNode) ->
	ActiveNodePath = io_lib:format("~s/~w", [Prefix, ActiveNode]),
	log("active node path: ~s", ActiveNodePath),
	case ezk:get(ConnectionPId, ActiveNodePath, active_node_changed) of
		{ok, {<<Address>>, _Stat}} ->
			NewWorkerState = WorkerModule:restart_idle(Address, WorkerState),
			{SelfState, NewWorkerState, idle};
		{error, no_dir} -> % node deleted while electing
			reelect({SelfState, WorkerState})
	end.


terminate(shutdown, {{ConnectionPId, WorkerModule, Prefix, NodeNum}, WorkerState, _Mode}) ->
	NodePath = io_lib:format("~s/~w", [Prefix, NodeNum]),
	ezk:delete(ConnectionPId, NodePath),
	WorkerModule:terminate(WorkerState),
    ok.

handle_info({nodes_changed, {_Path, child_chnaged, _N}}, {SelfState, WorkerState, leader}) ->
	reelect({SelfState, WorkerState});
handle_info({active_node_changed, {_Path, _Action, _N}}, {SelfState, WorkerState, idle}) ->
	reelect({SelfState, WorkerState});
handle_info(_, State) ->
	State.



handle_call(no_call_allowed, _From, _State) ->
	error.

handle_cast(no_cast_allowed, _State) ->
	error.

code_change(_, _, _) ->
	error.
