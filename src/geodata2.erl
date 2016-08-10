-module(geodata2).

%% API
-export([
	start/0
	, lookup/2
	, lookup/3
	, stop/0
	, start_pool/5
	, start_pool/4
	, stop_pool/1
	, reload_base/1
	, reload_base/2
	, state/1
]).
-include("geodata2.hrl").

-type deep_list() :: [char() | atom() | deep_list()].

-type name_all() :: string() | atom() | deep_list() | (RawFilename :: binary()).
%%
-spec lookup(atom(), any()) -> {ok, list()|map()} | not_found | {error, Reason :: term()}.
lookup(Pool, IP) ->
	geodata2_worker:lookup(maybe_worker(Pool), IP).

lookup(Pool, IP, Schema) ->
	geodata2_worker:lookup(maybe_worker(Pool), IP, Schema).



start() ->
	true = ensure_started(geodata2).



-spec start_pool(atom()|{proc, atom()}, list()|map(), name_all()|undefined, list()|map|undefined) -> {error, term()} | ok.
start_pool(Name, PoolArgs, File, Schema) ->
	start_pool(local, Name, PoolArgs, File, Schema).

-spec start_pool(global|local, atom()|{proc, atom()}, list()|map(), name_all()|undefined, list()|map|undefined) -> {error, term()} | ok.
start_pool(Visibility, Name, PoolArgs, File, Schema) ->
	Size = geodata2_util:option(size, PoolArgs, ?DEFAULT_WORKERS_NUM),
	SupFlags = geodata2_util:option(sup_flags, PoolArgs, {one_for_one, 1, 5}),
	simplepool:start_pool(Visibility, Name, Size, geodata2_worker, [{file, File}, {schema, Schema}], SupFlags, geodata2_loader).



-spec stop_pool(atom()) -> {error, term()} | ok.
stop_pool(PoolName) ->
	simplepool:stop_pool(PoolName).




-spec reload_base(atom()|{proc, atom()}) -> ok|error|undefined.
reload_base(PoolName) ->
	Controller = maybe_controller(PoolName),
	geodata2_loader:reread(Controller).

-spec reload_base(atom()|{proc, atom()}, name_all()|undefined) -> ok|error|undefined.
reload_base(PoolName, File) ->
	Controller = maybe_controller(PoolName),
	geodata2_loader:reread(Controller, File).

-spec state(atom()|{proc, atom()}) -> {ok, undefined|name_all(), #mmdbmeta{}} | {error, name_all()}.
state(PoolName) ->
	Controller = maybe_controller(PoolName),
	geodata2_loader:state(Controller).

stop() ->
	application:stop(geodata2).

%%%===================================================================
%%% Internal functions
%%%===================================================================


ensure_deps_started(App) ->
	Deps = case application:get_key(App, applications) of
			   undefined -> [];
			   {_, V} -> V
		   end,
	lists:all(fun ensure_started/1,Deps).

ensure_started(App) ->
	application:load(App),
	ensure_deps_started(App)
		andalso case application:start(App) of
					ok ->
						true;
					{error, {already_started, App}} ->
						true;
					Else ->
						error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
						false
				end.

maybe_controller({proc, Controller}) -> Controller;
maybe_controller(Pool) -> simplepool:controller(Pool).

maybe_worker({proc, Worker}) -> Worker;
maybe_worker(Pool) -> simplepool:rand_worker(Pool).