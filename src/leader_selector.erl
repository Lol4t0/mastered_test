-module(leader_selector).
-behaviour(gen_server).
-include_lib("log.hrl").

-export([start/6, start_link/6, stop/1, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([behaviour_info/1]).
behaviour_info(callbacks) ->
    [{restart_leader,1}, {restart_idle,2}].


%% The startfunction.
start(ConnectionPId, WorkerModule, Address, Prefix, Order, WorkerRef) ->
    gen_server:start(?MODULE, [ConnectionPId, WorkerModule, Address, Prefix, Order, WorkerRef], []).

start_link(ConnectionPId, WorkerModule, Address, Prefix, Order, WorkerRef) ->
    gen_server:start_link(?MODULE, [ConnectionPId, WorkerModule, Address, Prefix, Order, WorkerRef], []).


init([ConnectionPId, WorkerModule, Address, Prefix, Order, WorkerRef]) ->
	SelfNodeName = io_lib:format("~s/~w", [Prefix, Order]),
	?LOG("Prefix: ~s", [Prefix]),
	{ok, _Path} = ezk:create(ConnectionPId, SelfNodeName, Address, e), % e for EPHEMERAL
	reelect({ConnectionPId, WorkerModule, Prefix, Order, WorkerRef}).

reelect(State = {ConnectionPId, _WorkerModule, Prefix, Order, _WorkerRef}) ->
	{ok, List} = ezk:ls(ConnectionPId, Prefix, self(), nodes_changed),
	ActiveNodes = lists:sort(lists:map (fun(<<Path>>) -> list_to_integer(Path) end, List)),
	?LOG("active nodes: ~w", [ActiveNodes]),
	_Return = case lists:takeWhile(fun(N) -> N < Order end) of
		[] ->
			?LOG("starting as leader", []),
			restart_as_leader(State);
		Nodes ->
			ActiveNode = lists:last(Nodes),
			?LOG("starting as idle with active node: ~w", [ActiveNode]),
			restart_as_idle(State, ActiveNode)
	end.

restart_as_leader(SelfState = {_ConnectionPId, WorkerModule, _Prefix, _Order, WorkerRef}) ->
	WorkerModule:restart_leader(WorkerRef),
	{SelfState, leader}.

restart_as_idle(SelfState = {ConnectionPId, WorkerModule, Prefix, _Order, WorkerRef}, ActiveNode) ->
	ActiveNodePath = io_lib:format("~s/~w", [Prefix, ActiveNode]),
	?LOG("active node path: ~s", [ActiveNodePath]),
	case ezk:get(ConnectionPId, ActiveNodePath, active_node_changed) of
		{ok, {<<Address>>, _Stat}} ->
			WorkerModule:restart_idle(WorkerRef, Address),
			{SelfState, idle};
		{error, no_dir} -> % node deleted while electing
			reelect(SelfState)
	end.


terminate(_Reason, {{ConnectionPId, _WorkerModule, Prefix, Order, _WorkerRef}, _Mode}) ->
	NodePath = io_lib:format("~s/~w", [Prefix, Order]),
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
