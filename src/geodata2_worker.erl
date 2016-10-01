-module(geodata2_worker).


-behaviour(gen_server).
-behaviour(gen_simplepool_worker).

-include("geodata2.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([
	simplepool_start_link/4
%%	, lookup/2, lookup_countrycity/2
	, lookup/2
	, lookup/3
]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {meta = undefined, data = undefined, schema = undefined}).

%%%===================================================================
%%% API
%%%===================================================================
simplepool_start_link(Visibility, Name, _Controller, Args) ->
	gen_server:start_link({Visibility, Name}, ?MODULE, Args, []).

lookup(Worker, IP) ->
	gen_server:call(Worker, {lookup, IP}).
lookup(Worker, IP, Schema) ->
	gen_server:call(Worker, {lookup, IP, Schema}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(Args) ->
	Schema = proplists:get_value(schema, Args, undefined),
	{ok, #state{schema = Schema}}.


handle_call({set, Meta, Data}, _From, State) ->
	{reply, ok, State#state{meta = Meta, data = Data}};

handle_call({lookup, IP}, _From, State) ->
	Result = do_lookup(IP, State),
	{reply, Result, State};

handle_call({lookup, IP, Schema}, _From, State) ->
	Result = do_lookup(IP, Schema, State),
	{reply, Result, State};


handle_call(_Request, _From, State) ->
	{reply, ok, State}.



handle_cast(_Request, State) ->
	{noreply, State}.



handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	ok.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_lookup(IP, #state{schema = Schema} = State) ->
	do_lookup(IP, Schema, State).

do_lookup(_IP, _Schema, #state{meta = undefined} = _State) ->
	not_loaded;

do_lookup(IP, Schema, #state{meta = Meta, data = Data} = _State) ->
	case geodata2_ip:make_ip(IP) of
		{ok, Bits, IPV} ->
			Result = geodata2_format:lookup(Meta, Data, Bits, IPV),
			filter_data(Result, Schema);
		Else ->
			Else
	end.



lookup_key(Rec, Key, Default) when is_binary(Key) ->
	case lists:keyfind(Key, 1, Rec) of
		{_, Val} -> Val;
		_ -> Default
	end;

lookup_key(Rec, [InnerSchema | _], _Default) when is_list(Rec), is_list(InnerSchema) ->
	[
		lists:foldl(
			fun({Name, InnerPath, InnerDefault}, Map) ->
				Map#{Name => lookup_key(R, InnerPath, InnerDefault)}
			end,
			#{},
			InnerSchema
		)

		|| R <- Rec
	];


lookup_key(Rec, [{list, Key} | Path], Default) when is_list(Rec), is_binary(Key) ->
	[lookup_key(R, [Key | Path], Default) || R <- Rec];


lookup_key(_Rec, [{list, _Key} | _Path], Default) ->
	Default;

lookup_key(Rec, [Key | Path], Default) ->
	case lists:keyfind(Key, 1, Rec) of
		{_, Val} when Path == [] -> Val;
		{_, Rec2} -> lookup_key(Rec2, Path, Default);
		_ -> Default
	end.


filter_data({ok, Rec}, Schema) when is_list(Schema) ->
	{ok, lists:foldl(
		fun({Name, Path, Default}, Map) ->
			Map#{Name => lookup_key(Rec, Path, Default)}
		end,
		#{},
		Schema
	)};

filter_data({ok, Rec}, map) ->
	{ok, maps:from_list(Rec)};
%%
%%filter_data({ok, Rec}, _) ->
%%	Rec;

filter_data(Result, _) ->
	Result.



%%==in legacy geoip bases==
%%is_satellite_provider -> <<"A2">>;
%%is_anonymous_proxy -> <<"A1">>;
%%true -> ISO