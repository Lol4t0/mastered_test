-module(leader_selector).
-behaviour(gen_server).

-export([start/6, start_link/6, stop/1, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([behaviour_info/1]).
behaviour_info(callbacks) ->
    [{restart_leader,1}, {restart_idle,2}].


log(Msg, Args) ->
	io:format("MASTERED_TEST::" ++ Msg ++ "~n", Args).

%% The startfunction.
start(ConnectionPId, WorkerModule, Address, Prefix, NodeNum, WorkerRef) ->
    gen_server:start(?MODULE, [ConnectionPId, WorkerModule, Address, Prefix, NodeNum, WorkerRef], []).

start_link(ConnectionPId, WorkerModule, Address, Prefix, NodeNum, WorkerRef) ->
    gen_server:start_link(?MODULE, [ConnectionPId, WorkerModule, Address, Prefix, NodeNum, WorkerRef], []).


init([ConnectionPId, WorkerModule, Address, Prefix, NodeNum, WorkerRef]) ->
	SelfNodeName = io_lib:format("~s/~w", [Prefix, NodeNum]),
	log("Prefix: ~s", [Prefix]),
	{ok, _Path} = ezk:create(ConnectionPId, SelfNodeName, Address, e), % e for EPHEMERAL
	reelect({ConnectionPId, WorkerModule, Prefix, NodeNum, WorkerRef}).

reelect(State = {ConnectionPId, _WorkerModule, Prefix, NodeNum, _WorkerRef}) ->
	{ok, List} = ezk:ls(ConnectionPId, Prefix, self(), nodes_changed),
	ActiveNodes = lists:sort(lists:map (fun(<<Path>>) -> list_to_integer(Path) end, List)),
	log("active nodes: ~w", [ActiveNodes]),
	_Return = case lists:takeWhile(fun(N) -> N < NodeNum end) of
		[] ->
			log("starting as leader", []),
			restart_as_leader(State);
		Nodes ->
			ActiveNode = lists:last(Nodes),
			log("starting as idle with active node: ~w", [ActiveNode]),
			restart_as_idle(State, ActiveNode)
	end.

restart_as_leader(SelfState = {_ConnectionPId, WorkerModule, _Prefix, _NodeNum, WorkerRef}) ->
	WorkerModule:restart_leader(WorkerRef),
	{SelfState, leader}.

restart_as_idle(SelfState = {ConnectionPId, WorkerModule, Prefix, _NodeNum, WorkerRef}, ActiveNode) ->
	ActiveNodePath = io_lib:format("~s/~w", [Prefix, ActiveNode]),
	log("active node path: ~s", [ActiveNodePath]),
	case ezk:get(ConnectionPId, ActiveNodePath, active_node_changed) of
		{ok, {<<Address>>, _Stat}} ->
			WorkerModule:restart_idle(WorkerRef, Address),
			{SelfState, idle};
		{error, no_dir} -> % node deleted while electing
			reelect(SelfState)
	end.


terminate(_Reason, {{ConnectionPId, _WorkerModule, Prefix, NodeNum, _WorkerRef}, _Mode}) ->
	NodePath = io_lib:format("~s/~w", [Prefix, NodeNum]),
	ezk:delete(ConnectionPId, NodePath),
    ok.

stop(Ref) ->
	gen_server:call(Ref, stop).

handle_info({nodes_changed, {_Path, child_chnaged, _N}}, {State, leader}) ->
	reelect(State);
handle_info({active_node_changed, {_Path, _Action, _N}}, {State, idle}) ->
	reelect(State);
handle_info(_, State) ->
	State.

handle_call(stop, _From, State) ->
	{stop, normal, State}.

handle_cast(no_cast_allowed, _State) ->
	error.

code_change(_, _, _) ->
	error.
