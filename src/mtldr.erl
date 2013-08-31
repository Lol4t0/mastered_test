-module(mtldr).
-export([start/0, stop/0, restart/0, ask/1]).

start() ->
	code:add_path("../ezk/ebin"),
	application:load(ezk),
	application:start(ezk),
	application:load(mastered_test),
	application:start(mastered_test).

stop() ->
	application:stop(mastered_test),
	application:unload(mastered_test),
	application:stop(ezk),
	application:unload(ezk).

restart() ->
	stop(),
	start().

ask(N) ->
	{ok, S} = mastered_test:ask_worker(N),
	io:format("~s", [S]).
