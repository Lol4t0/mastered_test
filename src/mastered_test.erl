-module(mastered_test).
-include_lib("log.hrl").
-behaviour(supervisor).
-export([start_link/4, init/1]).

start_link(ConnectionPid, Orders, NumberOfWorkers, Address) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {ConnectionPid, Orders, NumberOfWorkers, Address}).



init({ConnectionPid, Orders, NumberOfWorkers, Address}) ->

	Workers = create_workers({Address, ConnectionPid}, Orders, NumberOfWorkers, 1),
	{ok,{{one_for_one, 100, 600}, Workers}}.

create_workers(_, _, 0, _)
	-> [];
create_workers(Params, [Order | Orders], WorkersLeft, WorkerNum) ->
	[create_worker(Params, WorkerNum, Order) | create_workers(Params, Orders, WorkersLeft-1, WorkerNum+1)].

create_worker({Address, ConnectionPid}, WorkerNum, Order) ->
	?LOG("Creating worker ~w", [WorkerNum]),
	Id = {worker, WorkerNum},
	Func = {worker, start_link, [ConnectionPid, Order, {Address, WorkerNum}]},
	{Id, Func, permanent, 1000, worker, [worker]}.
