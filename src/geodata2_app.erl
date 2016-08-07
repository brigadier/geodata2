-module(geodata2_app).
-behaviour(application).
-include("geodata2.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	start_pools(),
	geodata2_sup:start_link().

stop(_State) ->
	ok.




start_pools() ->
	Pools = application:get_env(geodata2, pools, []),
	lists:foreach(
		fun({PoolName, [PoolOptions, Args]}) ->
			Size = proplists:get_value(size, PoolOptions, ?DEFAULT_WORKERS_NUM),
			SupFlags = proplists:get_value(sup_flags, PoolOptions, {one_for_one, 1, 5}),
			ok = simplepool:start_pool(
				PoolName,
				Size,
				geodata2_worker,
				Args,
				SupFlags,
				geodata2_loader
			)
		end,
		Pools
	).