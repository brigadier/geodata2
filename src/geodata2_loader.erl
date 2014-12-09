-module(geodata2_loader).

-behaviour(gen_server).

%% API
-export([start_link/2, reread/1, reread/2]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {meta, file, name}).
-include("geodata2.hrl").
%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, File) ->
	PName = geodata2_poolsup:loader_name(Name),
	gen_server:start_link({local, PName}, ?MODULE, {File, Name}, []).


reread(Name) ->
	PName = geodata2_poolsup:loader_name(Name),
	ok = gen_server:call(PName, {reread}),
	erlang:garbage_collect(whereis(PName)), %% call this outside of callback
	ok.
reread(Name, File) ->
	PName = geodata2_poolsup:loader_name(Name),
	ok = gen_server:call(PName, {reread, File}),
	erlang:garbage_collect(whereis(PName)), %% call this outside of callback
	ok.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init({File, Name}) ->
	{ok, MetaRec} = load(File, Name),
	{ok, #state{meta = MetaRec, file = File, name = Name}}.


handle_call({reread}, _From, #state{file = File, name = Name} = State) ->
	{ok, MetaRec} = load(File, Name),
	{reply, ok, State#state{meta = MetaRec}};

handle_call({reread, File}, _From, #state{name = Name} = State) ->
	{ok, MetaRec} = load(File, Name),
	{reply, ok, State#state{meta = MetaRec, file = File}};

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

load(File, Name) ->
	{ok, Data} = file:read_file(File),
	{ok, Meta} = geodata2_format:meta(Data),

	Workers = geodata2_poolsup:worker_names(Name),
	lists:foreach(
		fun(Worker) ->
			ok = gen_server:call(Worker, {set, Meta, Data}),
			erlang:garbage_collect(whereis(Worker))
		end,
		Workers
	),
	{ok, Meta}.



