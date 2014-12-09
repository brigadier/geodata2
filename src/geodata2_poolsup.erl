-module(geodata2_poolsup).

-behaviour(supervisor).

-include("geodata2.hrl").
%% API
-export([start_link/2, loader_name/1, worker_name/2, worker_names/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Name, File) ->
	supervisor:start_link({local, Name}, ?MODULE, {Name, File}).

loader_name(Name) ->
	list_to_atom("_geodata2_" ++ atom_to_list(Name) ++ "_loader_").

worker_name(Name, Id) ->
	list_to_atom("_geodata2_" ++ atom_to_list(Name) ++ "_worker_" ++ integer_to_list(Id)).

worker_names(Name) ->
	[worker_name(Name, I) || I <- lists:seq(1, ?WORKERS_NUM)].


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({Name, File}) ->
	RestartStrategy = one_for_all,
	MaxRestarts = 10,
	MaxSecondsBetweenRestarts = 5,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Loader = {loader_name(Name), {geodata2_loader, start_link, [Name, File]},
		permanent, 2000, worker, [geodata2_loader]},
	Workers = [
		{WName, {geodata2_worker, start_link, [WName]},
			permanent, 2000, worker, [geodata2_worker]}
		|| WName <- worker_names(Name)],
	{ok, {SupFlags, Workers ++ [Loader]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
