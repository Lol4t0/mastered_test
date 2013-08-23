-module(mastered_test).
-export([create_workers/2, tell_worker/3]).


create_worker(K, ServerId) -> spawn(fun() -> worker:worker_main(K, ServerId) end).

create_workers(0, _, _) -> [];
create_workers(N, K, ServerId) -> [create_worker(K, ServerId) | create_workers(N-1, K+1, ServerId)].

create_workers(N, ServerId) -> create_workers(N, 1, ServerId).

tell_worker(K, Workers, Request) -> 
	Pid = lists:nth(K, Workers),
	Pid ! {self(), Request},
	receive 
		{ok, Request, K, ServerId} -> io:format("Got valid response from server ~s.~n", [ServerId]), ok;
		X ->io:format("Invalid response.~n"), {error, X}
	end.


	