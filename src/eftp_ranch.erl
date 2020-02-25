-module(eftp_ranch).

-export([
    start_link/0
]).

start_link() ->
    ranch:start_listener(
        ?MODULE,
        ranch_tcp,
        [{port, application:get_env(eftp, ftp_srv_port, 2121)}],
        eftp,
        []
    ).

%% How to handle stopping better....
