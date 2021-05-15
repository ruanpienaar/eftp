-module(eftp).

%% https://en.wikipedia.org/wiki/List_of_FTP_commands

-export([
    start_link/3,
    init/3
]).

-define(CRLF, <<"\r\n">>).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

start_link(Ref, Transport, Opts) ->
    io:format("SYS: start~p\n", [Ref]),
    {ok, proc_lib:spawn_link(fun() ->
        init(Ref, Transport, Opts)
    end)}.

%% TODO: login free / anon based access

init(Ref, Transport, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = send(Socket, Transport, <<"200 Erlang FTP Server">>),
    State = #{
        socket => Socket,
        transport => Transport,
        want_next => undefined,
        features => [], %% TODO,
        cwd => list_to_binary(element(2, file:get_cwd())), %% TODO ( get initial value )
        session_mode => undefined, %% TODO: active/passive
        host => <<"192,168,0,36">>, %% TODO
        port_1 => eftp_config:listening_port_1(),
        port_2 => eftp_config:listening_port_2()
    },
    loop(Socket, Transport, <<>>, State).

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
            case send(Socket, Transport, Response) of
                ok ->
                    handle_commands(Socket, Transport, Rest, NState);
                close ->
                    close
            end;
        close ->
            close;
        error ->
            close
    end.

send(_, _, []) ->
    ok;
send(_, _, [close|_]) ->
    close;
send(Socket, Transport, [D|T]) ->
    ok = send(Socket, Transport, D),
    send(Socket, Transport, T);
send(Socket, Transport, Data) when is_binary(Data) ->
    SendData = list_to_binary([Data, ?CRLF]),
    io:format("SND: \t\t~p\n", [SendData]),
    Transport:send(Socket, SendData).

%% TODO: Find a smarter/faster way to tokenize the commands
split(B) ->
    split(B, []).

split(B, Commands) ->
    case binary:split(B, ?CRLF) of
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
    ?LOG_INFO(#{ state => State }),
    {ok, State, <<"534 ">>};
%% USER
%%     331 User name okay, need password.
%%     332 Need account for login.
handle(<<"USER ", User/bits>>, State) ->
    ?LOG_INFO(#{ state => State }),
    %% TODO: check user
    {ok, State#{
        want_next => <<"PASS ">>,
        user => User
    }, <<"331 ">>};
handle(<<"PASS ", _Password/bits>>, #{ want_next := <<"PASS ">> } = State) ->
    ?LOG_INFO(#{ state => State }),
    %% TODO: check password
    {ok, State#{
        want_next => undefined
    }, <<"230 User successfully logged in">>};
%% PWD
handle(<<"PWD">>, #{ cwd := CWD } = State) ->
    ?LOG_INFO(#{ state => State }),
    {ok, State, list_to_binary([<<"257 ">>, <<"\"">>, CWD, <<"\"">>, <<" is working directory.">>])}; %% TODO: make gen bin func
%% CWD
%%     250
%%     500, 501, 502, 421, 530, 550
handle(<<"CWD ", Pathname/bits>>, State) ->
    ?LOG_INFO(#{ state => State }),
    % io:format("Change working directory to ~p\n", [Pathname]),
    {ok, State#{
        cwd => Pathname
    }, <<"250 Requested file action okay, completed">>};
handle(<<"MDTM", YYYYMMDDHHMMSS/bits>>, State) ->
    ?LOG_INFO(#{ state => State }),
    {ok, State#{
        mdtm => YYYYMMDDHHMMSS
    }, <<"253 Attributes changed okay.">>};
%% TYPE A|I
%%     200
%%     421
%%     500, 501, 504, 530
handle(<<"TYPE ", Type/bits>>, State) ->
    ?LOG_INFO(#{ state => State }),
    % io:format("file_representation_type ~p\n", [Type]),
    {ok, State#{
        file_representation_type => Type
    }, <<"200 file representation type set">>};
%% FEAT
%%     211
%%     500, 501, 502, 530
handle(<<"FEAT">>, #{ features := _Features } = State) -> %% TODO
    ?LOG_INFO(#{ state => State }),
    {ok, State, [<<"211 ">>, <<"211 END">>]};
handle(<<"QUIT">>, State) ->
    ?LOG_INFO(#{ state => State }),
    close;
handle(<<"SYST">>, #{ want_next := undefined } = State) ->
    ?LOG_INFO(#{ state => State }),
    {ok, State, list_to_binary([<<"200 ">>, get_os_name()])};
handle(<<"PASV">>, #{
        host := Host,
        port_1 := Port1,
        port_2 := Port2
    } = State) ->
    ?LOG_INFO(#{ state => State }),
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
% <<"PORT 127,0,0,1,216,106">>
handle(<<"PORT ", HostAndPort/bits>>, State) -> % (h1,h2,h3,h4,p1,p2)
    ?LOG_INFO(#{ state => State }),
    [_H1, _H2, _H3, _H4, P1, P2] = binary:split(HostAndPort, <<",">>, [global, trim_all]), %% TODO: use HOST
    Port = (binary_to_integer(P1) * 256) + binary_to_integer(P2), % (p1 * 256) + p2 = data port.
    % {ok, Socket} = gen_tcp:connect("localhost", Port, []),
    io:format("~s\n", [os:cmd("netstat -an | grep " ++ integer_to_list(Port))]),
    % {ok, _} = eftp_sup:start_child(eftp_ranch_999, Port), %% TODO: create sup id
    {ok, State, <<"200 ">>};
% TODO: (with pathname) - handle(<<"LIST">> [<SP> <pathname>] <CRLF>, State) -> {ok, State, <<"">>};
handle(<<"LIST">>, #{ cwd := Cwd } = State) ->
    %% Tried looking at MLSx response ( MLST, MLSD)
    %% can't quite get Filezilla happy....
    ?LOG_INFO(#{ state => State }),
    {ok, CwdFileList} = file:list_dir(Cwd),
    % {ok, State, binary:list_to_bin([<<"200 ">>] ++ lists:map(fun(File) -> File ++ " " end, CwdFileList)) };
    _ResponseMessage = lists:map(
        fun(File) ->

            % 16> file:raw_read_file_info("rebar3").
            % {ok,#file_info{size = 943940,type = regular,
            %                access = read_write,
            %                atime = {{2021,5,15},{15,21,33}},
            %                mtime = {{2021,2,25},{1,5,40}},
            %                ctime = {{2021,2,27},{17,53,22}},
            %                mode = 33261,links = 1,major_device = 16777220,
            %                minor_device = 0,inode = 45896410,uid = 501,gid = 20}}

            {ok, RawInfo} = file:raw_read_file_info(File),
            #file_info{ size = Size } = RawInfo,
            Facts = [
                "Size=", Size
                % "Modify" /
                % "Create" /
                % "Type" /
                % "Unique" /
                % "Perm" /
                % "Lang" /
                % "Media-Type" /
                % "CharSet"
            ],

            % binary:list_to_bin([Cwd, "/", File, " "])
            Facts
        end,
        CwdFileList
    ),
    % "250" [ SP response-message ] CRLF
    {ok, State,
        [
            <<"150 Opening ASCII mode data connection for MLS.">>,
            <<"type=cdir;unique=AQkAAAAAAAABCAAA; /">>,
            <<"type=dir;unique=AQkAAAAAAAABEAAA; file1">>,
            <<"226 Listing completed.">>
        ]
        % [
        %     <<"150 ">>,
        %     binary:list_to_bin([<<"250 ">>, <<"Size=943940 ">>, Cwd, "/rebar3", ?CRLF]),
        %     <<"226 ">>
        %     % ,
        %     % close
        % ]
    };
handle(Cmd, State) ->
    ?LOG_INFO(#{ state => State }),
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

get_os_name() ->
  io_lib:format("~s", [ os:cmd("uname") -- "\n" ]).





% Command RFC Description
% ABOR    Abort an active file transfer.
% ACCT    Account information.
% ADAT  RFC 2228  Authentication/Security Data
% ALLO    Allocate sufficient disk space to receive a file.
% APPE    Append (with create)
% AUTH  RFC 2228  Authentication/Security Mechanism
% AVBL  Streamlined FTP Command Extensions  Get the available space
% CCC RFC 2228  Clear Command Channel
% CDUP    Change to Parent Directory.
% CONF  RFC 2228  Confidentiality Protection Command
% CSID  Streamlined FTP Command Extensions  Client / Server Identification
% CWD RFC 697 Change working directory.
% DELE    Delete file.
% DSIZ  Streamlined FTP Command Extensions  Get the directory size
% ENC RFC 2228  Privacy Protected Channel
% EPRT  RFC 2428  Specifies an extended address and port to which the server should connect.
% EPSV  RFC 2428  Enter extended passive mode.
% FEAT  RFC 2389  Get the feature list implemented by the server.
% HELP    Returns usage documentation on a command if specified, else a general help document is returned.
% HOST  RFC 7151  Identify desired virtual host on server, by name.
% LANG  RFC 2640  Language Negotiation
% LIST    Returns information of a file or directory if specified, else information of the current working directory is returned.
% LPRT  RFC 1639  Specifies a long address and port to which the server should connect.
% LPSV  RFC 1639  Enter long passive mode.
% MDTM  RFC 3659  Return the last-modified time of a specified file.
% MFCT  The 'MFMT', 'MFCT', and 'MFF' Command Extensions for FTP  Modify the creation time of a file.
% MFF The 'MFMT', 'MFCT', and 'MFF' Command Extensions for FTP  Modify fact (the last modification time, creation time, UNIX group/owner/mode of a file).
% MFMT  The 'MFMT', 'MFCT', and 'MFF' Command Extensions for FTP  Modify the last modification time of a file.
% MIC RFC 2228  Integrity Protected Command
% MKD   Make directory.
% MLSD  RFC 3659  Lists the contents of a directory if a directory is named.
% MLST  RFC 3659  Provides data about exactly the object named on its command line, and no others.
% MODE    Sets the transfer mode (Stream, Block, or Compressed).
% NLST    Returns a list of file names in a specified directory.
% NOOP    No operation (dummy packet; used mostly on keepalives).
% OPTS  RFC 2389  Select options for a feature (for example OPTS UTF8 ON).
% PASS    Authentication password.
% PASV    Enter passive mode.
% PBSZ  RFC 2228  Protection Buffer Size
% PORT    Specifies an address and port to which the server should connect.
% PROT  RFC 2228  Data Channel Protection Level.
% PWD   Print working directory. Returns the current directory of the host.
% QUIT    Disconnect.
% REIN    Re initializes the connection.
% REST  RFC 3659  Restart transfer from the specified point.
% RETR    Retrieve a copy of the file
% RMD   Remove a directory.
% RMDA  Streamlined FTP Command Extensions  Remove a directory tree
% RNFR    Rename from.
% RNTO    Rename to.
% SITE    Sends site specific commands to remote server (like SITE IDLE 60 or SITE UMASK 002). Inspect SITE HELP output for complete list of supported commands.
% SIZE  RFC 3659  Return the size of a file.
% SMNT    Mount file structure.
% SPSV  FTP Extension Allowing IP Forwarding (NATs) Use single port passive mode (only one TCP port number for both control connections and passive-mode data connections)
% STAT    Returns information on the server status, including the status of the current connection
% STOR    Accept the data and to store the data as a file at the server site
% STOU    Store file uniquely.
% STRU    Set file transfer structure.
% SYST    Return system type.
% THMB  Streamlined FTP Command Extensions  Get a thumbnail of a remote image file
% TYPE    Sets the transfer mode (ASCII/Binary).
% USER    Authentication username.
% XCUP  RFC 775 Change to the parent of the current working directory
% XMKD  RFC 775 Make a directory
% XPWD  RFC 775 Print the current working directory
% XRCP  RFC 743
% XRMD  RFC 775 Remove the directory
% XRSQ  RFC 743
% XSEM  RFC 737 Send, mail if cannot
% XSEN  RFC 737 Send to terminal
