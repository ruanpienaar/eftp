-module(eftp_config).

-export([
    listening_port_1/0,
    listening_port_2/0
]).

listening_port_1() ->
    application:get_env(eftp, ?MODULE, 2121).

listening_port_2() ->
    application:get_env(eftp, ?MODULE, 2122).
