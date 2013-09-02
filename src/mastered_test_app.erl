
-module(mastered_test_app).
-include_lib("log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Nodes} = application:get_env(cnodes),
	{ok, NumberOfWorkers} = application:get_env(number_of_workers),
	{ok,Servers} = application:get_env(zkservers),
	{ok, ConnectionPid} = ezk:start_connection(Servers),
	Orders = make_orders(0,node(),Nodes,NumberOfWorkers),
	?LOG("order list ~p",[Orders]),
	case mastered_test_sup:start_link(ConnectionPid, Orders) of
		{ok, SupPid} ->
			{ok, SupPid, ConnectionPid};
		Error ->
			{error, Error}
	end.

stop(ConnectionPid) ->
	?LOG("terminating application", []),
	ezk:end_connection(ConnectionPid, shutdown),
	ok.


make_orders(I,Node,[Node|_],Workers)->
	[{N,(I+N) rem Workers} || N<-lists:seq(0,Workers-1)];
make_orders(I,Node,[_|T],Workers)->
	make_orders(I+1,Node,T,Workers).