-module(mastered_test).
-include_lib("log.hrl").
-behaviour(gen_server).
-export([start/3, start_link/3, init/1, terminate/2, stop/0, ask_worker/1,
		 handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

start(Orders, NumberOfWorkers, Address) ->
	gen_server:start({local, mastered_test}, ?MODULE, [Orders, NumberOfWorkers, Address], []).

start_link(Orders, NumberOfWorkers, Address) ->
	gen_server:start_link({local, mastered_test}, ?MODULE, [Orders, NumberOfWorkers, Address], []).



init([Orders, NumberOfWorkers, Address]) ->
	{ok, ConnectionPid} = ezk:start_connection(),
	?LOG("Ezk started with PID ~w", [ConnectionPid] ),
	try create_workers({Address, ConnectionPid}, Orders, NumberOfWorkers, 1) of % 1 for first worker
		Workers ->
		State = {ConnectionPid, Workers},
		{ok, State}
	catch
		Error ->
		{stop, Error}
	end.


create_workers(_, _, 0, _)
	-> [];
create_workers(Params, [Order | Orders], WorkersLeft, WorkerNum) ->
	[create_worker(Params, WorkerNum, Order) | create_workers(Params, Orders, WorkersLeft-1, WorkerNum+1)].

create_worker({Address, ConnectionPid}, WorkerNum, Order) ->
	?LOG("Creating worker ~w", [WorkerNum]),
	case (worker:start(ConnectionPid, Order, {Address, WorkerNum})) of
		{ok, Pid} ->
			?LOG("Success!", []),
			Pid;
		Error ->
			throw({can_t_create_worker, WorkerNum, Error})
	end.




terminate(Reason, {ConnectionPid, Workers}) ->
	?LOG("WWW", []),
	delete_workers(Workers),
	ezk:end_connection(ConnectionPid, Reason).

delete_workers([]) -> ok;
delete_workers([Worker|Others]) ->
	worker:stop(Worker),
	delete_workers(Others).

stop() ->
	exit(whereis(mastered_test), shutdown).


ask_worker(WorkerNum) ->
	gen_server:call(mastered_test, {ask_worker, WorkerNum}).


handle_call({ask_worker, WorkerNum}, _From, State = {_CPid, Workers}) ->
	Ref = lists:nth(WorkerNum, Workers),
	Reply = worker:client_request(Ref),
	{reply, Reply, State}.



handle_cast(_, _) ->
	error.

code_change(_, _, _) ->
	error.

handle_info(_,_) ->
	error.

