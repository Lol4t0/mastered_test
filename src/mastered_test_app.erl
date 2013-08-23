
-module(mastered_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) -> io:format("hi there!!!"), mastered_test:start(StartArgs).
stop(_State) -> ok.
