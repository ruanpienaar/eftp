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
                    id       => eftp_ranch,                   % mandatory
                    start    => {eftp_ranch, start_link, []}, % mandatory
                    restart  => permanent,                    % optional
                    shutdown => 1000,                         % optional
                    type     => worker,                       % optional
                    modules  => [eftp_ranch]                  % optional
                }
            ]
        }
    }.
