
-module(mastered_test_app).
-include_lib("log.hrl").

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
	{ok, ConnectionPid} = ezk:start_connection(),
	case mastered_test:start_link(ConnectionPid, Orders, NumberOfWorkers, Address) of
		{ok, SupPid} ->
			{ok, SupPid, ConnectionPid};
		Error ->
			{error, Error}
	end.

stop(ConnectionPid) ->
	?LOG("terminating application", []),
	ezk:end_connection(ConnectionPid, shutdown),
	ok.
