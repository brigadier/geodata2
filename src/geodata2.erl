-module(geodata2).

%% API
-export([lookup/2, start/0, start/1, stop/0, open_base/2, close_base/1, reload_base/1, reload_base/2, lookup_geocity/2]).
-include("geodata2.hrl").

-type deep_list() :: [char() | atom() | deep_list()].

-type name_all() :: string() | atom() | deep_list() | (RawFilename :: binary()).

-spec lookup(atom(), any()) -> {ok, Result::list()} | not_found | {error, Reason :: term()}.
lookup(Name, IP) ->
	geodata2_worker:lookup(Name, IP).

-spec lookup_geocity(atom(), any()) -> {ok, geocity()} | not_found | {error, Reason :: term()}.
lookup_geocity(Name, IP) ->
	geodata2_worker:lookup_countrycity(Name, IP).






-spec start() -> ok | {error, Reason :: term()}.
start() ->
	application:start(geodata2).


-spec start([{atom(), name_all()}]) -> ok | {error, Reason :: term()}.
start(Bases) ->
	ok = application:load(geodata2),
	ok = application:set_env(geodata2, bases, Bases),
	start().

%% @doc
%% Open an mmdb file 'File' with handle 'Name'. Handle must be unique.
-spec open_base(atom(), name_all()) -> supervisor:startchild_ret().
open_base(Name, File) when is_atom(Name) ->
	geodata2_sup:start_child(Name, File).

%% @doc
%% Close the mmdb file referenced by handle 'Name' (atom).
-spec close_base(atom()) -> ok | {error, Reason :: term()}.
close_base(Name) when is_atom(Name) ->
	geodata2_sup:stop_child(Name).

%% @doc
%% Reread from the disk an mmdb file referenced by handle 'Name'.
-spec reload_base(atom()) -> ok | {error, Reason :: term()}.
reload_base(Name) when is_atom(Name) ->
	geodata2_loader:reread(Name).

%% @doc
%% Use another file with name 'File' in the process referernced by handle 'Name' (atom).
-spec reload_base(atom(), name_all()) -> ok | {error, Reason :: term()}.
reload_base(Name, File) when is_atom(Name) ->
	geodata2_loader:reread(Name, File).





stop() ->
	application:stop(geodata2).

%%%===================================================================
%%% Internal functions
%%%===================================================================