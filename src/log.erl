-module(log).
-export([put/3]).

put(Module, Msg, Args) ->
		io:format("MASTERED_TEST:~w:" ++ Msg ++ "~n", [Module | Args]).
