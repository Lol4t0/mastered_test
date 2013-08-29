-module(mastered_test).
-behaviour(gen_server).
-export([start/3, start_link/3, init/1, terminate/2, stop/1, ask_worker/1,
		 handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

start(Orders, NumberOfWorkers, Address) ->
	gen_server:start({local, mastered_test}, ?MODULE, [Orders, NumberOfWorkers, Address], []).

start_link(Orders, NumberOfWorkers, Address) ->
	gen_server:start_link({local, mastered_test}, ?MODULE, [Orders, NumberOfWorkers, Address], []).



init([Orders, NumberOfWorkers, Address]) ->
	{ok, ConnectionPid} = ezk:start_connection(),
	_Return = {
		ConnectionPid,
		create_workers({Address, ConnectionPid}, Orders, NumberOfWorkers, 1) % 1 for first worker
		}.

create_workers(_, _, 0, _)
	-> [];
create_workers(Params, [Order | Orders], WorkersLeft, WorkerNum) ->
	[create_worker(Params, WorkerNum, Order) | create_workers(Params, Orders, WorkersLeft-1, WorkerNum+1)].

create_worker({Address, ConnectionPid}, WorkerNum, Order) ->
	worker:start_link(ConnectionPid, Order, {Address, WorkerNum}).




terminate(Reason, {ConnectionPid, Workers}) ->
	delete_workers(Workers),
	ezk:end_connection(ConnectionPid, Reason).

delete_workers([]) -> ok;
delete_workers([Worker|Others]) ->
	worker:stop(Worker),
	delete_workers(Others).

stop(Ref) ->
	gen_server:call(Ref, stop).



ask_worker(WorkerNum) ->
	gen_server:call(mastered_test, {ask_worker, WorkerNum}).




handle_call(stop, _From, State) ->
	{stop, normal, State};

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

