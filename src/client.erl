-module(client).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {socket, user}).
-compile (export_all).

-include ("user.hrl").

%% ===================================================================
%% APIs
%% ===================================================================

start_link(User) ->
    gen_server:start_link(?MODULE, [User], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([User]) ->
    Password = uri_encode(User#user.password),
    DeviceName = User#user.device,
    Result = httpc:request(post,
                          {"https://localhost:8080/server/login", [],
                           "application/x-www-form-urlencoded",
                           <<"phone=", (User#user.phone)/binary, "&password=", Password/binary, "&device=", DeviceName/binary>>},
                          [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    {<<"server">>, ServerBin} = lists:keyfind(<<"server">>, 1, Attrs),
                    {<<"port">>, PortBin} = lists:keyfind(<<"port">>, 1, Attrs),
                    {<<"user">>, UserInfo} = lists:keyfind(<<"user">>, 1, Attrs),
                    {<<"id">>, UserId} = lists:keyfind(<<"id">>, 1, UserInfo),
                    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, UserInfo),
                    NewUser = User#user{id = UserId, token = Token},
                    UserIdBin = erlang:integer_to_binary(UserId),
                    Server = erlang:binary_to_list(ServerBin),
                    Port = erlang:binary_to_integer(PortBin),
                    % io:format("~p connect to ~p:~p~n", [self(), Server, Port]),
                    case ssl:connect (Server, Port, [{packet,0}, {active, true}], infinity) of
                        {ok, Socket} ->
                            ok;
                        _ ->
                            TokenStr = erlang:binary_to_list(Token),
                            Result1 = httpc:request(post,
                                                  {"https://localhost:8080/server/failed", [{"Cookie", "token=" ++ TokenStr}], "", <<>>},
                                                  [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
                            case Result1 of
                                {ok, {{"HTTP/1.1",200,"OK"}, _, Body1}} ->
                                    {ok, [{<<"response">>, Attrs1}]} = toml:binary_2_term(Body1),
                                    case lists:keyfind(<<"status">>, 1, Attrs1) of
                                        {<<"status">>, 0} ->
                                            {<<"server">>, ServerBin1} = lists:keyfind(<<"server">>, 1, Attrs1),
                                            {<<"port">>, PortBin1} = lists:keyfind(<<"port">>, 1, Attrs1),
                                            Server1 = erlang:binary_to_list(ServerBin1),
                                            Port1 = erlang:binary_to_integer(PortBin1),
                                            % io:format("~p connect to ~p:~p~n", [self(), Server1, Port1]),
                                            {ok, Socket} = ssl:connect (Server1, Port1, [{packet,0}, {active, true}]);
                                        Any ->
                                            io:format("Error http:~p~n", [Any]),
                                            Socket = undefined
                                    end;
                                Any ->
                                    io:format("Error im:~p~n", [Any]),
                                    Socket = undefined
                            end
                    end,
                    State = #state{socket = Socket, user = NewUser},
                    Msg = <<"[[r]] id = \"abc_01\" t = \"login\" [r.user] id = ", UserIdBin/binary,
                            " d = \"", (NewUser#user.device)/binary,
                            "\" token = \"", Token/binary, "\"">>,
                    ssl:send(State#state.socket, Msg),
                    % io:format ("~p===client login!~n", [self()]),
                    case UserId + 1 < 10000 of
                        true ->
                            erlang:send_after(2000, self(), {send_msg, UserId + 1});
                        _ ->
                            ok
                    end,
                    {ok, State};
                _ ->
                   {stop, http_request_failed}
            end;
        _ ->
            io:format("Can not connect to http server~n"),
            {stop, http_connect_failed}
    end.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({ssl, Socket, Data}, #state{socket = Socket} = State) ->
    io:format ("~p===Got msg(~p): ~p~n", [self(), os:timestamp(), Data]),
    {ok, TomlList} = toml:binary_2_term(Data),
    process_package(TomlList, State),
    {noreply, State};
handle_info({ssl_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, ssl_closed, State};
handle_info({send_msg, UserId}, State) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    DeviceName = State#state.user#user.device,
    A = utility:timestamp(),
    ABin = erlang:integer_to_binary(A),
    Id = <<"msg_id_", ABin/binary>>,
    Msg = <<"[[m]] id = \"", Id/binary, "\" t = \"text\" c = \"hello\" to = ", UserIdBin/binary, " d = \"", DeviceName/binary, "\"">>,
    ssl:send(State#state.socket, Msg),
    % io:format ("~p client send msg!~p~n", [self(), os:timestamp()]),
    % erlang:send_after(2000, self(), {send_msg, UserId}),
    {noreply, State};
handle_info({send_group_msg, GroupId}, State) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    DeviceName = State#state.user#user.device,
    Msg = <<"[[gm]] id = \"a_03\" t = \"text\" c = \"hello\" g_id = ", GroupIdBin/binary, " d = \"", DeviceName/binary, "\"">>,
    ssl:send(State#state.socket, Msg),
    io:format ("===client send msg!~n"),
    {noreply, State};
handle_info({search_user, Phone}, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    Result = httpc:request(get,
                           {"https://localhost:8080/user/phone/" ++ Phone, [{"Cookie", "token=" ++ TokenStr}]},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            io:format("~p Got user: ~p~n", [self(), Body]);
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info({add_friend, Id}, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    IdStr = erlang:integer_to_list(Id),
    Result = httpc:request(post,
                           {"https://localhost:8080/contact/" ++ IdStr,
                            [{"Cookie", "token=" ++ TokenStr}],
                            "application/x-www-form-urlencoded",
                            <<"ask=fuck you">>},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    io:format("User ~p add friend to user ~p success~n", [State#state.user#user.id, Id]);
                _ ->
                    {<<"reason">>, Reason} = lists:keyfind(<<"reason">>, 1, Attrs),
                    io:format("Error Reason: ~p~n", [Reason])
            end;
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info({accept_friend, Id}, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    IdStr = erlang:integer_to_list(Id),
    Result = httpc:request(put,
                           {"https://localhost:8080/contact/" ++ IdStr,
                            [{"Cookie", "token=" ++ TokenStr}], [], <<>>},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    io:format("User ~p accept friend to user ~p success~n", [State#state.user#user.id, Id]);
                _ ->
                    {<<"reason">>, Reason} = lists:keyfind(<<"reason">>, 1, Attrs),
                    io:format("Error Reason: ~p~n", [Reason])
            end;
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info({delete_friend, Id}, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    IdStr = erlang:integer_to_list(Id),
    Result = httpc:request(delete,
                           {"https://localhost:8080/contact/" ++ IdStr,
                            [{"Cookie", "token=" ++ TokenStr}], [], <<>>},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    io:format("User ~p delete friend to user ~p success~n", [State#state.user#user.id, Id]);
                _ ->
                    {<<"reason">>, Reason} = lists:keyfind(<<"reason">>, 1, Attrs),
                    io:format("Error Reason: ~p~n", [Reason])
            end;
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info(create_group, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    Result = httpc:request(post,
                           {"https://localhost:8080/group",
                            [{"Cookie", "token=" ++ TokenStr}],
                            "application/x-www-form-urlencoded",
                            <<"members[]=2&members[]=3&name=group1">>},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    io:format("User ~p create group success~n", [State#state.user#user.id]);
                _ ->
                    {<<"reason">>, Reason} = lists:keyfind(<<"reason">>, 1, Attrs),
                    io:format("Error Reason: ~p~n", [Reason])
            end;
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info({delete_group, GroupId}, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    GroupIdStr = erlang:integer_to_list(GroupId),
    Result = httpc:request(delete,
                           {"https://localhost:8080/group/" ++ GroupIdStr,
                            [{"Cookie", "token=" ++ TokenStr}], [], <<>>},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    io:format("User ~p delete group ~p success~n", [State#state.user#user.id, GroupIdStr]);
                _ ->
                    {<<"reason">>, Reason} = lists:keyfind(<<"reason">>, 1, Attrs),
                    io:format("Error Reason: ~p~n", [Reason])
            end;
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info({create_group_member, GroupId, Key}, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    GroupIdStr = erlang:integer_to_list(GroupId),
    KeyEncoded = uri_encode(Key),
    Result = httpc:request(post,
                           {"https://localhost:8080/group/" ++ GroupIdStr ++ "/member",
                            [{"Cookie", "token=" ++ TokenStr}],
                            "application/x-www-form-urlencoded",
                            <<"g_key=", KeyEncoded/binary>>},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    io:format("User ~p join group ~p success~n", [State#state.user#user.id, GroupIdStr]);
                _ ->
                    {<<"reason">>, Reason} = lists:keyfind(<<"reason">>, 1, Attrs),
                    io:format("Error Reason: ~p~n", [Reason])
            end;
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info({delete_group_member, GroupId}, State) ->
    TokenStr = erlang:binary_to_list(State#state.user#user.token),
    GroupIdStr = erlang:integer_to_list(GroupId),
    Result = httpc:request(delete,
                           {"https://localhost:8080/group/" ++ GroupIdStr ++ "/member",
                            [{"Cookie", "token=" ++ TokenStr}], [], <<>>},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(Body),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    io:format("User ~p quit group ~p success~n", [State#state.user#user.id, GroupIdStr]);
                _ ->
                    {<<"reason">>, Reason} = lists:keyfind(<<"reason">>, 1, Attrs),
                    io:format("Error Reason: ~p~n", [Reason])
            end;
        _ ->
            io:format("Can not connect to http server~n")
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

uri_encode(Uri) when is_binary(Uri) ->
    erlang:list_to_binary(http_uri:encode(erlang:binary_to_list(Uri)));
uri_encode(Uri) ->
    erlang:list_to_binary(http_uri:encode(Uri)).


get_offline_msg(Token) ->
    TokenStr = erlang:binary_to_list(Token),
    Result = httpc:request(get,
                           {"https://localhost:8080/offline", [{"Cookie", "token=" ++ TokenStr}]},
                           [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            [ResponseBin, ZippedData] = re:split(Body, "\r\n"),
            {ok, Response} = toml:binary_2_term(ResponseBin),
            {<<"response">>,Attrs} = lists:keyfind(<<"response">>, 1, Response),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    MsgListBin = zlib:unzip(ZippedData),
                    {ok, MsgList} = toml:binary_2_term(MsgListBin),
                    httpc:request(delete,
                                  {"https://localhost:8080/offline", [{"Cookie", "token=" ++ TokenStr}]},
                                  [{ssl, [{cacertfile, "priv/ssl/cowboy-ca.crt"}]}], []),
                    {ok, MsgList};
                _ ->
                   {stop, http_request_failed}
            end;
        _ ->
            io:format("Can not connect to http server~n"),
            {stop, http_connect_failed}
    end.


process_package([H|T], #state{socket = Socket, user = User} = State) ->
    DeviceName = User#user.device,
    case H of
        {<<"rr">>, Attrs} ->
            case lists:keyfind(<<"id">>, 1, Attrs) of
                {<<"id">>, <<"abc_01">>} ->
                    case lists:keyfind(<<"s">>, 1, Attrs) of
                        {<<"s">>, 0} ->
                            % io:format ("~p Login success, id is ~p~n", [self(), User#user.id]),
                            % io:format("~p Got offline msg lists ~p~n", [self(), MsgList]),
                            {ok, MsgList} = get_offline_msg(User#user.token);
                        {<<"s">>, 1} ->
                            {<<"r">>, Reason} = lists:keyfind(<<"r">>, 1, Attrs),
                            io:format ("~p Login failed, reason is ~p~n", [self(), Reason]);
                        _ ->
                            io:format ("Login Error~n")
                    end;
                _ ->
                    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
                    Ack = <<"[[a]] id=\"", MsgId/binary, "\" d = \"", DeviceName/binary, "\"">>,
                    % io:format ("===Send ack: ~p~n", [Ack]),
                    ssl:send(Socket, Ack),
                    io:format("~p Unkown response~n", [self()]),
                    ok
            end;
        {<<"m">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\" d = \"", DeviceName/binary, "\"">>,
            % io:format ("~p Send ack: ~p~n", [self(), Ack]),
            ssl:send(Socket, Ack);
        {<<"gm">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\" d = \"", DeviceName/binary, "\"">>,
            % io:format ("~p Send ack: ~p~n", [self(), Ack]),
            ssl:send(Socket, Ack);
        {<<"a">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\" d = \"", DeviceName/binary, "\"">>,
            % send ack back
            % io:format ("~p Msg id=~p send success~n", [self(), MsgId]),
            ssl:send(Socket, Ack);
        {<<"r">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\" d = \"", DeviceName/binary, "\"">>,
            io:format ("===Send ack: ~p~n", [Ack]),
            % io:format("~p Got request: ~p~n", [self(), {<<"r">>, Attrs}]),
            ssl:send(Socket, Ack);
        {<<"n">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\" d = \"", DeviceName/binary, "\"">>,
            io:format ("===Send ack: ~p~n", [Ack]),
            % io:format("~p Got notification: ~p~n", [self(), {<<"n">>, Attrs}]),
            ssl:send(Socket, Ack);
        {<<"gn">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\" d = \"", DeviceName/binary, "\"">>,
            % io:format ("===Send ack: ~p~n", [Ack]),
            % io:format("~p Got group notification: ~p~n", [self(), {<<"gn">>, Attrs}]),
            ssl:send(Socket, Ack);
        _ ->
            io:format("~p Unkown message: ~p~n", [self(), H]),
            ok
    end,
    process_package(T, State);
process_package([], _) ->
    ok.