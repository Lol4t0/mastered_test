-module(worker).
-include_lib("log.hrl").
-behaviour(gen_server).
-behaviour(leader_selector).
-export([start_link/2,
		 restart_leader/1, restart_idle/2,
		 client_request/1,
		 init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-define(PREFIX, "/mastered_test/election/worker~w").

-record(state,{worker,selector,state}).
start_link(ConnectionPid, Order) ->
	gen_server:start_link(?MODULE, [ConnectionPid, Order], []).


init([ConnectionPid, {Worker,Order}]) ->
	?LOG("starting worker~w", [{Worker,Order}]),
	process_flag(trap_exit, true),
	WorkerZNodePath = lists:flatten(io_lib:format(?PREFIX, [Worker])),
	case leader_selector:start_link(ConnectionPid, ?MODULE,WorkerZNodePath, Order, self()) of
		{ok, SelectorRef} ->
			?LOG("ok", []),
			{ok,#state{worker=Worker,selector=SelectorRef,state=starting}};
		Error ->
			?LOG("Failed~w", [Error]),
			{stop, Error}
	end.

terminate(_Reason,#state{selector=SelectorRef,worker=WorkerNum}) ->
	?LOG("Deleting worker ~w", [WorkerNum]),
	leader_selector:stop(SelectorRef).


restart_leader(Ref) ->
	?LOG("starting worker as leader", []),
	gen_server:cast(Ref, leader).

restart_idle(Ref, Address) ->
	?LOG("starting worker as idle", []),
	gen_server:cast(Ref, {idle, Address}).

client_request(Ref) ->
	gen_server:call(Ref, client_request).

handle_call(client_request, _From, State ) ->
	Answer = case State of
		#state{state=starting} ->
			{error, "Worker is not initialized yet."};
		#state{worker=WorkerNum,state=leader}->
			{ok, io_lib:format("The Worker ~w on the node ~p is answering you, human. 42.~n", [WorkerNum, node()])};
		#state{worker=WorkerNum,state={idle, ActiveAddress}}->
			{ok, io_lib:format("Worker ~w on the node ~p is not in a position to answer you, Human. Ask node ~s, please~n.",
						   [WorkerNum,node(), ActiveAddress])}
		end,
	{reply, Answer, State}.


handle_cast(S,State) ->
	{noreply, State#state{state=S}}.


code_change(_, _, _) ->
	error.

handle_info(_,_) ->
	error.
