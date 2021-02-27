-module(eftp_ranch).

-export([
    start_link/2
]).

start_link(Id, Port) ->
    ranch:start_listener(
        Id,
        ranch_tcp,
        [{port, Port}],
        eftp,
        []
    ).

%% How to handle stopping better....
