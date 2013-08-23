-module(worker).
-export([worker_main/2]).

worker_main(K, ServerId) ->
	receive 
		{Sender, Request} -> 
			Sender ! {ok, Request, K, ServerId}
	end,
	worker_main(K, ServerId).