-module(eftp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    {ok,
        {
            % Restart strategy
            #{
                strategy  => one_for_one, % optional
                intensity => 1,           % optional
                period    => 10           % optional
            },
            % Children
            [
                % Child spec
                #{
                    id       => eftp_ranch_1,
                    start    => {eftp_ranch, start_link, [eftp_ranch_1, eftp_config:listening_port_1()]},
                    restart  => permanent,
                    shutdown => 30000,
                    type     => worker,
                    modules  => [eftp_ranch]
                },
                % Child spec
                #{
                    id       => eftp_ranch_2,
                    start    => {eftp_ranch, start_link, [eftp_ranch_2, eftp_config:listening_port_2()]},
                    restart  => permanent,
                    shutdown => 30000,
                    type     => worker,
                    modules  => [eftp_ranch]
                }
            ]
        }
    }.
