-module(mastered_test_sup).
-include_lib("log.hrl").
-behaviour(supervisor).
-export([start_link/2, init/1, ask/1]).

start_link(ConnectionPid, Orders) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {ConnectionPid, Orders}).



init({ConnectionPid, Orders}) ->

	Workers = create_workers(ConnectionPid, Orders),
	{ok,{{one_for_one, 100, 600}, Workers}}.

create_workers(_,[])
	-> [];
create_workers(Params, [Order | Orders]) ->
	[create_worker(Params,Order) | create_workers(Params, Orders)].

create_worker(ConnectionPid,Order) ->
	?LOG("Creating worker ~w", [Order]),
	Id = {worker, Order},
	Func = {worker, start_link, [ConnectionPid, Order]},
	{Id, Func, permanent, 1000, worker, [worker]}.

ask(WorkerNum) ->
	Children = supervisor:which_children(?MODULE),
	[TheOne] = lists:filter(
		fun (X) ->
			case X of
				{{worker, WorkerNum}, _, _, _} -> true;
				_Else -> false
			end
		end,
		Children),
	{_, Ref, _, _} = TheOne,
	worker:client_request(Ref).
