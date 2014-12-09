-module(geodata2_sup).
-behaviour(supervisor).

-export([start_link/0, stop_child/1, start_child/2]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Name, File) ->
	supervisor:start_child(geodata2_sup, child_spec(Name, File)).

stop_child(Name) ->
	supervisor:terminate_child(geodata2_sup, Name).

init([]) ->
	Bases = case application:get_env(geodata2, bases) of
				undefined -> [];
				{ok, V} -> V
			end,

	BasesSpec = [child_spec(N, F) ||{N, F} <- Bases],

	RestartStrategy = one_for_one,
	MaxRestarts = 1,
	MaxSecondsBetweenRestarts = 1,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	{ok, {SupFlags, BasesSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


child_spec(Name, File) when is_atom(Name)->
	{Name, {geodata2_poolsup, start_link, [Name, File]},
		temporary, 2000, supervisor, [geodata2_poolsup]}.