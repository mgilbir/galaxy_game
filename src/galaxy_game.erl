%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2]).

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    [create_planet(Planet) || Planet <- Planets],
    [create_shield(Shield) || Shield <- Shields],
    [create_alliance(Alliance) || Alliance <- Alliances].

%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->
    lists:foreach(fun(Planet) -> teardown_planet(Planet) end, Planets).

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
	[attack_planet(Action) || Action <- Actions],
    lists:all(fun (P) ->
            PPid = whereis(P),
            PPid /= undefined andalso erlang:is_process_alive(PPid)
    end, Planets).

%% Private

-spec create_planet(planet()) -> ok.
create_planet(Planet) ->
	Pid = spawn(
		fun() -> 
			io:format("Planet ~p created~n", [Planet]),
			loop(Planet)
		end),
	register(Planet, Pid),
	ok.


loop(Planet) ->
	receive 
		teardown ->
			io:format("Planet ~p destroyed~n", [Planet]),
			unregister(Planet),
			ok;
		create_shield ->
			process_flag(trap_exit, true),
			io:format("Planet ~p shielded~n", [Planet]),
			loop(Planet);
		{create_alliance, Ally_planet} ->
			io:format("Alliance formed between ~p and ~p~n", [Planet, Ally_planet]),
			link(Ally_planet),
			loop(Planet);
		{'EXIT', _FromPid, Reason} ->
			% We are under attack but we have a shield
			case Reason of 
				laser ->
					io:format("Planet ~p resists the attack thanks to the shield~n", [Planet]);
				nuclear ->
					Planet ! teardown
			end,
			loop(Planet)
	end.

-spec teardown_planet(planet()) -> ok.
teardown_planet(Planet) ->
	whereis(Planet) ! teardown.

-spec create_shield(shield()) -> ok.
create_shield(Shield) ->
	whereis(Shield) ! create_shield,
	ok.

-spec create_alliance(alliance()) -> ok.
create_alliance({PlanetA, PlanetB}) ->
	io:format("Form alliance between ~p and ~p~n", [PlanetA, PlanetB]),
	Pid_PlanetA = whereis(PlanetA),
	
	case Pid_PlanetA of
		undefined ->
			io:format("Couldn't find planet ~p~n", [PlanetA]);
		Pid ->
			Pid ! {create_alliance, whereis(PlanetB)}
	end,
	ok.

-spec attack_planet(attack()) -> ok.
attack_planet({Attack_type, Planet}) ->
	exit(whereis(Planet), Attack_type),
	ok.

