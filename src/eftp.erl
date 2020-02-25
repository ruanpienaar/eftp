-module(eftp).
-export([
    start_link/3,
    init/3
]).

start_link(Ref, Transport, Opts) ->
    {ok, proc_lib:spawn_link(fun() ->
        init(Ref, Transport, Opts)
    end)}.

%% TODO: login free / anon based access

init(Ref, Transport, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    Transport:send(Socket, <<"200 Erlang FTP Server\r\n">>),
    loop(Socket, Transport, <<>>, #{ want_next => undefined }).

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
    case handle(Socket, Transport, Cmd, State) of
        {ok, NState} ->
            handle_commands(Socket, Transport, Rest, NState);
        close ->
            close;
        error ->
            close
    end.

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

handle(Socket, Transport, <<"USER ", User/bits>>, State) ->

% 331 User name okay, need password.
% 332 Need account for login.

    io:format("Username ~p\n", [User]),
    ok = Transport:send(Socket, <<"331 \r\n">>),
    {ok, State#{ want_next => <<"PASS ">>}};
% handle(Socket, Transport, <<"PASS">> <SP> <password> <CRLF>, State) ->
handle(Socket, Transport, <<"PASS ", Password/bits>>, #{ want_next := <<"PASS ">> } = State) ->
    io:format("Password ~p\n", [Password]),
    ok = Transport:send(Socket, <<"230 User successfully logged in\r\n">>),
    {ok, State#{ want_next => undefined }};
% handle(Socket, Transport, <<"ACCT">> <SP> <account-information> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"CWD ">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"CDUP">> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"SMNT">> <SP> <pathname> <CRLF>, State) -> {ok, State};
handle(Socket, Transport, <<"QUIT">>, State) ->
    close;
% handle(Socket, Transport, <<"REIN">> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"PORT">> <SP> <host-port> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"PASV">> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"TYPE">> <SP> <type-code> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"STRU">> <SP> <structure-code> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"MODE">> <SP> <mode-code> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"RETR">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"STOR">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"STOU">> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"APPE">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"ALLO">> <SP> <decimal-integer> [<SP> R <SP> <decimal-integer>] <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"REST">> <SP> <marker> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"RNFR">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"RNTO">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"ABOR">> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"DELE">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"RMD ">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"MKD ">> <SP> <pathname> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"PWD ">> <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"LIST">> [<SP> <pathname>] <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"NLST">> [<SP> <pathname>] <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"SITE">> <SP> <string> <CRLF>, State) -> {ok, State};
handle(Socket, Transport, <<"SYST">>, #{ want_next := undefined } = State) ->
    ok = Transport:send(Socket, list_to_binary([<<"200 ">>,io_lib:format("~s", [os:cmd("uname")])])),
    {ok, State};
% handle(Socket, Transport, <<"STAT">> [<SP> <pathname>] <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"HELP">> [<SP> <string>] <CRLF>, State) -> {ok, State};
% handle(Socket, Transport, <<"NOOP">> <CRLF>, State) -> {ok, State};
handle(Socket, Transport, Cmd, State) ->
    io:format("unhandled cmd ~p\n", [Cmd]),
    ok = Transport:send(Socket, <<"500\r\n">>),
    error.


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
