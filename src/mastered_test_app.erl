
-module(mastered_test_app).
-include_lib("log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Orders} = application:get_env(orders),
	{ok, NumberOfWorkers} = application:get_env(number_of_workers),
	{ok, Address} = application:get_env(address),
	mastered_test:start(Orders, NumberOfWorkers, Address).
prep_stop(_State) ->
	mastered_test:stop().

stop(_) ->
	ok.
