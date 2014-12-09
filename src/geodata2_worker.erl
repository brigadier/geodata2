-module(geodata2_worker).


-behaviour(gen_server).
-include("geodata2.hrl").
%% API
-export([start_link/1, lookup/2, lookup_countrycity/2]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {meta, data}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [], []).
lookup_countrycity(Name, IP) ->
	Worker = get_worker(Name, IP),
	gen_server:call(Worker, {lookup, IP, cs}).

lookup(Name, IP) ->
	Worker = get_worker(Name, IP),
	gen_server:call(Worker, {lookup, IP, all}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
	{ok, #state{}}.


handle_call({set, Meta, Data}, _From, State) ->
	{reply, ok, State#state{meta = Meta, data = Data}};

handle_call({lookup, IP, What}, _From, #state{meta = Meta, data = Data} = State) ->
	case geodata2_ip:make_ip(IP) of
		{ok, Bits, IPV} ->
			Result = geodata2_format:lookup(Meta, Data, Bits, IPV),
			{reply, filter_data(Result, What), State};
		Error ->
			{reply, Error, State}
	end;

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

get_worker(Name, Address) ->
	geodata2_poolsup:worker_name(Name, 1 + erlang:phash2(Address) band (?WORKERS_NUM - 1)).


filter_data({ok, Rec}, cs) ->
	RC = maybe_keyfind(<<"country">>, 1, Rec, undefined),
	ISO = maybe_keyfind(<<"iso_code">>, 1, RC, <<"XX">>),
	Loc =  maybe_keyfind(<<"location">>, 1, Rec, undefined),
	Long = maybe_keyfind(<<"longitude">>, 1, Loc, undefined),
	Lat = maybe_keyfind(<<"latitude">>, 1, Loc, undefined),
	City = maybe_keyfind(<<"city">>, 1, Rec, undefined),
	CityNames = maybe_keyfind(<<"names">>, 1, City, undefined),
	CityName = maybe_keyfind(<<"en">>, 1, CityNames, <<>>),
	GeonameID = maybe_keyfind(<<"geoname_id">>, 1, City, undefined),
	Traits = maybe_keyfind(<<"traits">>, 1, Rec, undefined),
	IsSat = maybe_keyfind(<<"is_satellite_provider">>, 1, Traits, false),
	IsAnonProxy = maybe_keyfind(<<"is_anonymous_proxy">>, 1, Traits, false),
	Country = if
				  IsSat -> <<"A2">>;
				  IsAnonProxy -> <<"A1">>;
				  true -> ISO
			  end,
	{ok, #geocity{country = Country, city = CityName, long = Long, lat = Lat, city_geoid = GeonameID}};

filter_data(Result, _) ->
	Result.

maybe_keyfind(Key, Pos, List, Default) when is_list(List) ->
	case lists:keyfind(Key, Pos, List) of
		{_, Val} -> Val;
		_ -> Default
	end;
maybe_keyfind(_, _, _, Default)  ->
	Default.



%%geodata2:open_base(b, "priv/GeoIP2-City-Test.mmdb").
%%IP = {81,2,69,192}.
%%geodata2:lookup(b, IP).

%%"{ done , Pos }"
%{done,3145728}


%DEBUG: geodata2_worker:110
%"DataOffset"
%  3153184



%% ip2long({B3, B2, B1, B0}) ->
%% 	{ok, (B3 bsl 24) bor (B2 bsl 16) bor (B1 bsl 8) bor B0, ?IPV4};
%% ip2long({W7, W6, W5, W4, W3, W2, W1, W0}) ->
%% 	{ok, (W7 bsl 112) bor (W6 bsl 96) bor (W5 bsl 80) bor (W4 bsl 64) bor
%% 		(W3 bsl 48) bor (W2 bsl 32) bor (W1 bsl 16) bor W0};
%% ip2long(<<Addr:32>>) ->
%% 	{ok, Addr, ?IPV4};
%% ip2long(<<Addr:128>>) ->
%% 	{ok, Addr, ?IPV6};
%% ip2long(N) when is_integer(N), N =< 16#FFFFFFFF ->
%% 	{ok, N, ?IPV4};
%% ip2long(N) when is_integer(N) ->
%% 	{ok, N, ?IPV6}.

