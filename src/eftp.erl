-module(eftp).
-export([
    start_link/3,
    init/3
]).

start_link(Ref, Transport, Opts) ->
    io:format("SYS: start~p\n", [Ref]),
    {ok, proc_lib:spawn_link(fun() ->
        init(Ref, Transport, Opts)
    end)}.

%% TODO: login free / anon based access

init(Ref, Transport, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = send(Socket, Transport, <<"200 Erlang FTP Server">>),
    loop(Socket, Transport, <<>>, #{
        socket => Socket,
        transport => Transport,
        want_next => undefined,
        features => [], %% TODO,
        cwd => list_to_binary(element(2, file:get_cwd())), %% TODO ( get initial value )
        session_mode => undefined, %% TODO: active/passive
        host => <<"192,168,0,36">>, %% TODO
        port_1 => eftp_config:listening_port_1(),
        port_2 => eftp_config:listening_port_2()
    }).

loop(Socket, Transport, Buffer, State) ->
    case Transport:recv(Socket, 0, 30000) of
        {ok, Data} ->
            {Commands, Rest} = split(<<Buffer/binary, Data/binary>>),
            case handle_commands(Socket, Transport, Commands, State) of
                {ok, NextState} ->
                    loop(Socket, Transport, Rest, NextState);
                close ->
                    io:format("The client disconnected~n")
            end;
        {error, _} ->
            io:format("The client disconnected~n")
    end.

handle_commands(_Socket, _Transport, [], State) ->
    {ok, State};
handle_commands(Socket, Transport, [Cmd|Rest], State) ->
    io:format("CMD: \t~p\n", [Cmd]),
    case handle(Cmd, State) of
        {ok, NState, Response} ->
            ok = send(Socket, Transport, Response),
            handle_commands(Socket, Transport, Rest, NState);
        close ->
            close;
        error ->
            close
    end.

send(Socket, Transport, Data) when is_list(Data) ->
    [ ok = send(Socket, Transport, D) || D <- Data ],
    ok;
send(Socket, Transport, Data) when is_binary(Data) ->
    io:format("SND: \t\t~p\n", [Data]),
    Transport:send(Socket, list_to_binary([Data, "\r\n"])).

%% TODO: Find a smarter/faster way to tokenize the commands
split(B) ->
    split(B, []).

split(B, Commands) ->
    case binary:split(B, <<"\r\n">>) of
        [Cmd, <<>>] ->
            {lists:reverse([Cmd|Commands]), <<>>};
        [Cmd, CmdMore] ->
            split(CmdMore, [Cmd|Commands]);
        [CmdMore] ->
            {lists:reverse(Commands), CmdMore}
    end.

% A 534 response code can be issued in response to any command that the server is unwilling
% to process due to its security policy. It is a permanent negative response,
% which means the client is discouraged from sending the command again since the server
% will respond with the same response code.
handle(<<"AUTH", _AuthType/bits>>, State) ->
    {ok, State, <<"534 ">>};
%% USER
%%     331 User name okay, need password.
%%     332 Need account for login.
handle(<<"USER ", User/bits>>, State) ->
    %% TODO: check user
    {ok, State#{
        want_next => <<"PASS ">>,
        user => User
    }, <<"331 ">>};
handle(<<"PASS ", _Password/bits>>, #{ want_next := <<"PASS ">> } = State) ->
    %% TODO: check password
    {ok, State#{
        want_next => undefined
    }, <<"230 User successfully logged in">>};
%% PWD
handle(<<"PWD">>, #{ cwd := CWD } = State) ->
    {ok, State, list_to_binary([<<"257 ">>, <<"\"">>, CWD, <<"\"">>, <<" is working directory.">>])}; %% TODO: make gen bin func
%% CWD
%%     250
%%     500, 501, 502, 421, 530, 550
handle(<<"CWD ", Pathname/bits>>, State) ->
    % io:format("Change working directory to ~p\n", [Pathname]),
    {ok, State#{
        cwd => Pathname
    }, <<"250 Requested file action okay, completed">>};
handle(<<"MDTM", YYYYMMDDHHMMSS/bits>>, State) ->
    {ok, State#{
        mdtm => YYYYMMDDHHMMSS
    }, <<"253 Attributes changed okay.">>};
%% TYPE A|I
%%     200
%%     421
%%     500, 501, 504, 530
handle(<<"TYPE ", Type/bits>>, State) ->
    % io:format("file_representation_type ~p\n", [Type]),
    {ok, State#{
        file_representation_type => Type
    }, <<"200 file representation type set">>};
%% FEAT
%%     211
%%     500, 501, 502, 530
handle(<<"FEAT">>, #{ features := _Features } = State) -> %% TODO
    {ok, State, [<<"211 ">>, <<"211 END">>]};
handle(<<"QUIT">>, _State) ->
    close;
handle(<<"SYST">>, #{ want_next := undefined } = State) ->
    {ok, State, list_to_binary([<<"200 ">>, io_lib:format("~s", [os:cmd("uname")])])}; %% TODO: make gen bin func
handle(<<"PASV">>, #{
        host := Host,
        port_1 := Port1,
        port_2 := Port2
    } = State) ->
    {ok, State#{
        session_mode => passive
    },
        list_to_binary([
            <<"200 ">>,
            <<"(">>,
            Host, <<",">>,
            integer_to_binary(Port1), <<",">>,
            integer_to_binary(Port2),
            <<")">>
        ])
    };
handle(Cmd, State) ->
    io:format("unhandled cmd ~p\n", [Cmd]),
    {ok, State, <<"500 ">>}.

% handle(<<"ACCT">> <SP> <account-information> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"CDUP">> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"SMNT">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"REIN">> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"PORT">> <SP> <host-port> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"PASV">> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"STRU">> <SP> <structure-code> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"MODE">> <SP> <mode-code> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"RETR">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"STOR">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"STOU">> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"APPE">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"ALLO">> <SP> <decimal-integer> [<SP> R <SP> <decimal-integer>] <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"REST">> <SP> <marker> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"RNFR">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"RNTO">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"ABOR">> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"DELE">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"RMD ">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"MKD ">> <SP> <pathname> <CRLF>, State) -> {ok, State, <<"">>};

% handle(<<"LIST">> [<SP> <pathname>] <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"NLST">> [<SP> <pathname>] <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"SITE">> <SP> <string> <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"STAT">> [<SP> <pathname>] <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"HELP">> [<SP> <string>] <CRLF>, State) -> {ok, State, <<"">>};
% handle(<<"NOOP">> <CRLF>, State) -> {ok, State, <<"">>};

      % 5.3.1.  FTP COMMANDS

      %    The following are the FTP commands:

      %       USER <SP> <username> <CRLF>
      %       PASS <SP> <password> <CRLF>
      %       ACCT <SP> <account-information> <CRLF>
      %       CWD  <SP> <pathname> <CRLF>
      %       CDUP <CRLF>
      %       SMNT <SP> <pathname> <CRLF>
      %       QUIT <CRLF>
      %       REIN <CRLF>
      %       PORT <SP> <host-port> <CRLF>
      %       PASV <CRLF>
      %       TYPE <SP> <type-code> <CRLF>
      %       STRU <SP> <structure-code> <CRLF>
      %       MODE <SP> <mode-code> <CRLF>
      %       RETR <SP> <pathname> <CRLF>
      %       STOR <SP> <pathname> <CRLF>
      %       STOU <CRLF>
      %       APPE <SP> <pathname> <CRLF>
      %       ALLO <SP> <decimal-integer>
      %           [<SP> R <SP> <decimal-integer>] <CRLF>
      %       REST <SP> <marker> <CRLF>
      %       RNFR <SP> <pathname> <CRLF>
      %       RNTO <SP> <pathname> <CRLF>
      %       ABOR <CRLF>
      %       DELE <SP> <pathname> <CRLF>
      %       RMD  <SP> <pathname> <CRLF>
      %       MKD  <SP> <pathname> <CRLF>
      %       PWD  <CRLF>
      %       LIST [<SP> <pathname>] <CRLF>
      %       NLST [<SP> <pathname>] <CRLF>
      %       SITE <SP> <string> <CRLF>
      %       SYST <CRLF>
      %       STAT [<SP> <pathname>] <CRLF>
      %       HELP [<SP> <string>] <CRLF>
      %       NOOP <CRLF>

      % 5.3.2.  FTP COMMAND ARGUMENTS

      %    The syntax of the above argument fields (using BNF notation
      %    where applicable) is:

      %       <username> ::= <string>
      %       <password> ::= <string>
      %       <account-information> ::= <string>
      %       <string> ::= <char> | <char><string>
      %       <char> ::= any of the 128 ASCII characters except <CR> and
      %       <LF>
      %       <marker> ::= <pr-string>
      %       <pr-string> ::= <pr-char> | <pr-char><pr-string>
      %       <pr-char> ::= printable characters, any
      %                     ASCII code 33 through 126
      %       <byte-size> ::= <number>
      %       <host-port> ::= <host-number>,<port-number>
      %       <host-number> ::= <number>,<number>,<number>,<number>
      %       <port-number> ::= <number>,<number>
      %       <number> ::= any decimal integer 1 through 255
      %       <form-code> ::= N | T | C
      %       <type-code> ::= A [<sp> <form-code>]
      %                     | E [<sp> <form-code>]
      %                     | I
      %                     | L <sp> <byte-size>
      %       <structure-code> ::= F | R | P
      %       <mode-code> ::= S | B | C
      %       <pathname> ::= <string>
      %       <decimal-integer> ::= any decimal integer
