-module(server_supervisor).
-author("Janusz").
-behavior(supervisor).
-version('1.0').

%% API
-export([start_link/1, init/1]).

start_link(Val) ->
  supervisor:start_link({local, serverSupervisor}, server_supervisor, Val),
  unlink(whereis(serverSupervisor)).

init(_) ->
  {ok, {
    #{ strategy => one_for_one, intensity => 2, period => 1 },
    [ #{
      id => 'gen_srv',
      start => {pollution_gen_server, start, []},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [pollution_gen_server]
    },
    #{
      id => 'add_multiple_measures',
      start => {pollution_value_collector_gen_statem, start_link, []},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [pollution_value_collector_gen_statem]
    }]
  }}.
