
-module(mastered_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Orders} = application:get_env(orders),
	{ok, NumberOfWorkers} = application:get_env(number_of_workers),
	{ok, Address} = application:get_env(address),
	{ok, Ref} = mastered_test:start(Orders, NumberOfWorkers, Address),
	Ref.
stop(Ref) ->
	mastered_test:stop(Ref).
