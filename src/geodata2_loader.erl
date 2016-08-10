-module(geodata2_loader).

-behaviour(gen_server).
-behaviour(gen_simplepool_worker).
-include_lib("eunit/include/eunit.hrl").
%% API
-export([
	reread/1
	, reread/2
	, simplepool_start_link/4
	, state/1
]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {meta, file, workers}).
-include("geodata2.hrl").
%%%===================================================================
%%% API
%%%===================================================================

simplepool_start_link(Visibility, Name, Workers, Args) ->
	{_, File} = lists:keyfind(file, 1, Args),
	gen_server:start_link({Visibility, Name}, ?MODULE, [Workers, File | Args], []).

reread(Name) ->
	Result = gen_server:call(Name, reread),
	erlang:garbage_collect(whereis(Name)),
	Result.

reread(Name, File) ->
	Result = gen_server:call(Name, {reread, File}),
	erlang:garbage_collect(whereis(Name)), %% call this outside of callback
	Result.

state(Name) ->
	gen_server:call(Name, state).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Workers, File | _]) ->
	case load(File, Workers) of
		error ->
			{ok, #state{meta = undefined, file = File, workers = Workers}};
		undefined ->
			{ok, #state{meta = undefined, file = undefined, workers = Workers}};
		{ok, MetaRec} ->
			{ok, #state{meta = MetaRec, file = File, workers = Workers}}
	end.

handle_call(state, _From, #state{file = File, meta = Meta} = State) ->
	Status = if
				File == undefined -> {ok, undefined, undefined};
				Meta == undefined -> {error, File};
				true -> {ok, File, Meta}
			end,
	{reply, Status, State};

handle_call(reread, _From, #state{file = File, workers = Workers} = State) ->
	case load(File, Workers) of
		error ->
			{reply, error, State#state{meta = undefined}};
		undefined ->
			{reply, undefined, State#state{meta = undefined}};
		{ok, MetaRec} ->
			{reply, ok, State#state{meta = MetaRec}}
	end;

handle_call({reread, File}, _From, #state{workers = Workers} = State) ->
	case load(File, Workers) of
		error ->
			{reply, error, State#state{meta = undefined, file = File}};
		undefined ->
			{reply, undefined, State#state{meta = undefined, file = File}};
		{ok, MetaRec} ->
			{reply, ok, State#state{meta = MetaRec, file = File}}
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
load(undefined, Workers) -> %%if we decide to start the app without file. it's ok.
	lists:foreach(
		fun(Worker) ->
			ok = gen_server:call(Worker, {set, undefined, undefined}),
			erlang:garbage_collect(whereis(Worker))
		end,
		Workers
	),
	undefined;

load(File, Workers) ->
	try
		{ok, Data} = file:read_file(File),
		{ok, Meta} = geodata2_format:meta(Data),
		lists:foreach(
			fun(Worker) ->
				ok = gen_server:call(Worker, {set, Meta, Data}),
				erlang:garbage_collect(whereis(Worker))
			end,
			Workers
		),
		{ok, Meta}
	catch
		_:_ -> error
	end.



