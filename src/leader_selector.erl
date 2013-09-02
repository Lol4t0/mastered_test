-module(leader_selector).
-behaviour(gen_server).
-include_lib("log.hrl").

-export([start_link/5, stop/1, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([behaviour_info/1]).
behaviour_info(callbacks) ->
    [{restart_leader,1}, {restart_idle,2}].


-record(state,{ezk,worker,path,order,worker_ref,state}).

%% The startfunction.
start_link(ConnectionPId, WorkerModule, Prefix, Order, WorkerRef) ->
    gen_server:start_link(?MODULE, {ConnectionPId, WorkerModule, Prefix, Order, WorkerRef}, []).


init({ConnectionPId, WorkerModule, Prefix, Order, WorkerRef}) ->
	process_flag(trap_exit, true),
	SelfNodeName = lists:flatten(io_lib:format("~s/~w", [Prefix, Order])),
	?LOG("Prefix: ~s", [Prefix]),
	case ezk:create(ConnectionPId, SelfNodeName,atom_to_binary(node(),utf8), e) of % e for EPHEMERAL
		{ok, _Path} ->
			{ok, reelect(#state{ezk=ConnectionPId,worker=WorkerModule,path=Prefix,order=Order,worker_ref=WorkerRef})};
		Error ->
			{stop, {can_t_create_node, Error, SelfNodeName}}
	end.

reelect(#state{ezk=ConnectionPId,path=Prefix,order=Order}=State) ->
	?LOG("reelecting. Self ~w. Prefix ~s", [self(), Prefix]),
	{ok, List} = ezk:ls(ConnectionPId, Prefix, self(), nodes_changed),
	ActiveNodes = lists:sort(lists:map (fun(Path) -> list_to_integer(binary_to_list(Path)) end, List)),
	?LOG("active nodes: ~w", [ActiveNodes]),
	_Return = case lists:takewhile(fun(N) -> N < Order end, ActiveNodes) of
		[] ->
			?LOG("starting as leader", []),
			restart_as_leader(State);
		Nodes ->
			ActiveNode = lists:last(Nodes),
			?LOG("starting as idle with active node: ~w", [ActiveNode]),
			restart_as_idle(State, ActiveNode)
	end.

restart_as_leader(#state{worker=WorkerModule,worker_ref=WorkerRef}=State) ->
	WorkerModule:restart_leader(WorkerRef),
	State#state{state=leader}.

restart_as_idle(#state{ezk=ConnectionPId,worker=WorkerModule,path=Prefix,worker_ref=WorkerRef}=State, ActiveNode) ->
	ActiveNodePath = lists:flatten(io_lib:format("~s/~w", [Prefix, ActiveNode])),
	?LOG("active node path: ~s", [ActiveNodePath]),
	case ezk:get(ConnectionPId, ActiveNodePath, self(), active_node_changed) of
		{ok, {Address, _Stat}} ->
			WorkerModule:restart_idle(WorkerRef, binary_to_list(Address)),
			State#state{state=idle};
		{error, no_dir} -> % node deleted while electing
			reelect(State)
	end.


terminate(_Reason, #state{ezk=ConnectionPId,path=Prefix,order=Order}) ->
	NodePath = lists:flatten(io_lib:format("~s/~w", [Prefix, Order])),
	?LOG("Deleting node ~s. Connection: ~w", [NodePath, ConnectionPId]),
	ezk:delete(ConnectionPId, NodePath),
    ok.

stop(Ref) ->
	gen_server:call(Ref, stop).

handle_info({nodes_changed, {_Path, child_changed, _N}},#state{state=leader}=State) ->
	{noreply, reelect(State)};
handle_info({active_node_changed, {_Path, _Action, _N}}, #state{state=idle}=State) ->
	{noreply, reelect(State)};
handle_info(M, State) ->
	io:format("WTFF!!! ~w", [M]),
	{noreply, State}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(no_cast_allowed, _State) ->
	error.

code_change(_, _, _) ->
	error.
