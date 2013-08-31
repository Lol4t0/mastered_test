-module(worker).
-include_lib("log.hrl").
-behaviour(gen_server).
-behaviour(leader_selector).
-export([start/3, start_link/3, stop/1,
		 restart_leader/1, restart_idle/2,
		 client_request/1,
		 init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-define(PREFIX, "/mastered_test/election/worker~w").

start(ConnectionPid, Order, {SelfAddress, WorkerNum}) ->
	gen_server:start(?MODULE, [ConnectionPid, Order, SelfAddress, WorkerNum], []).

start_link(ConnectionPid, Order, {SelfAddress, WorkerNum}) ->
	gen_server:start_link(?MODULE, [ConnectionPid, Order, SelfAddress, WorkerNum], []).

stop(Ref) ->
	gen_server:call(Ref, stop).



init([ConnectionPid, Order, SelfAddress, WorkerNum]) ->
	?LOG("starting worker~w", [WorkerNum]),
	WorkerZNodePath = lists:flatten(io_lib:format(?PREFIX, [WorkerNum])),
	case leader_selector:start(ConnectionPid, ?MODULE, SelfAddress, WorkerZNodePath, Order, self()) of
		{ok, SelectorRef} ->
			?LOG("ok", []),
			{ok, {SelfAddress, WorkerNum, SelectorRef, starting}};
		Throw ->
			?LOG("Failed~w", [Throw]),
			{stop, Throw}
	end.

terminate(_Reason, {_SelfAddress, WorkerNum, SelectorRef, _Mode}) ->
	?LOG("Deleting worker ~w", [WorkerNum]),
	leader_selector:stop(SelectorRef).


restart_leader(Ref) ->
	?LOG("starting worker as leader", []),
	R = gen_server:cast(Ref, leader),
	?LOG("done", []),
	R.

restart_idle(Ref, Address) ->
	?LOG("starting worker as idle", []),
	R = gen_server:cast(Ref, {idle, Address}),
	?LOG("done", []),
	R.

client_request(Ref) ->
	gen_server:call(Ref, client_request).


handle_call(stop, _From, S) ->
{stop, normal, ok, S};

handle_call(client_request, _From, State ) ->
	Answer = case State of
		{_SelfAddress, _WorkerNum, _SelectorRef, starting} ->
			{error, "Worker is not initialized yet."};
		{SelfAddress, WorkerNum, _SelectorRef, leader} ->
			{ok, io_lib:format("The Worker ~w on the node ~s is answering you, human. 42.~n", [WorkerNum, SelfAddress])};
		{SelfAddress, WorkerNum, _SelectorRef, {idle, ActiveAddress}} ->
			{ok, io_lib:format("Worker ~w on the node ~s is not in a position to answer you, Human. Ask node ~s, please~n.",
						   [WorkerNum, SelfAddress, ActiveAddress])}
		end,
	{reply, Answer, State}.


handle_cast(S, {SelfAddress, WorkerNum, SelectorRef, _Mode}) ->
	NewState = {SelfAddress, WorkerNum, SelectorRef, S},
	{noreply, NewState}.


code_change(_, _, _) ->
	error.

handle_info(_,_) ->
	error.
