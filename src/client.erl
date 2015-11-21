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
    Result = httpc:request(post,
                          {"https://localhost:8080/server/login", [],
                           "application/x-www-form-urlencoded",
                           <<"phone=", (User#user.phone)/binary, "&password=", Password/binary>>},
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
                    io:format("~p connect to ~p:~p~n", [self(), Server, Port]),
                    case gen_tcp:connect (Server, Port, [{packet,0}, {active, true}]) of
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
                                            io:format("~p connect to ~p:~p~n", [self(), Server1, Port1]),
                                            {ok, Socket} = gen_tcp:connect (Server1, Port1, [{packet,0}, {active, true}]);
                                        _ ->
                                            Socket = undefined
                                    end;
                                _ ->
                                    Socket = undefined
                            end
                    end,
                    State = #state{socket = Socket, user = NewUser},
                    Msg = <<"[[r]] id = \"abc_01\" t = \"login\" [r.user] id = ", UserIdBin/binary,
                            " device = \"", (NewUser#user.device)/binary,
                            "\" token = \"", Token/binary, "\"">>,
                    gen_tcp:send(State#state.socket, Msg),
                    io:format ("~p===client login!~n", [self()]),
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

handle_info ({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    io:format ("~p===Got msg(~p): ~p~n", [self(), os:timestamp(), Data]),
    {ok, TomlList} = toml:binary_2_term(Data),
    process_package(TomlList, State),
    {noreply, State};
handle_info ({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, tcp_closed, State};
handle_info({send_msg, UserId}, State) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    Msg = <<"[[m]] id = \"a_02\" c = \"hello\" to = ", UserIdBin/binary>>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send msg!~p~n", [self(), os:timestamp()]),
    {noreply, State};
handle_info({send_group_msg, GroupId}, State) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    Msg = <<"[[gm]] id = \"a_03\" c = \"hello\" group = ", GroupIdBin/binary>>,
    gen_tcp:send(State#state.socket, Msg),
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
    IdBin = erlang:integer_to_binary(Id),
    Msg = <<"[[r]] id = \"a_12344\" t = \"add_contact\" message = \"fuck you\" to = ", IdBin/binary>>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send r!~n", [self()]),
    {noreply, State};
handle_info({accept_friend, Id}, State) ->
    IdBin = erlang:integer_to_binary(Id),
    Msg = <<"[[r]] id = \"a_12345\" t = \"accept_contact\" to = ", IdBin/binary>>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send r!~n", [self()]),
    {noreply, State};
handle_info({delete_friend, Id}, State) ->
    IdBin = erlang:integer_to_binary(Id),
    Msg = <<"[[r]] id = \"a_12346\" t = \"delete_contact\" to = ", IdBin/binary>>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send r!~n", [self()]),
    {noreply, State};
handle_info({create_group, Members}, State) ->
    {ok, Msg} = toml:term_2_binary({<<"r">>,
                                    [{<<"members">>,[State#state.user#user.id|Members]},
                                     {<<"name">>,<<"fuck">>},
                                     {<<"t">>,<<"create_group">>},
                                     {<<"id">>,<<"c_01">>}]}),
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send r!~n", [self()]),
    {noreply, State};
handle_info({delete_group, GroupId}, State) ->
    {ok, Msg} = toml:term_2_binary({<<"r">>,
                                    [{<<"group_id">>,GroupId},
                                     {<<"t">>,<<"delete_group">>},
                                     {<<"id">>,<<"c_01">>}]}),
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send r!~n", [self()]),
    {noreply, State};
handle_info({create_group_member, GroupId, Key}, State) ->
    {ok, Msg} = toml:term_2_binary({<<"r">>,
                                    [{<<"group_id">>,GroupId},
                                     {<<"key">>,Key},
                                     {<<"t">>,<<"create_group_member">>},
                                     {<<"id">>,<<"d_01">>}]}),
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send r!~n", [self()]),
    {noreply, State};
handle_info({delete_group_member, GroupId}, State) ->
    {ok, Msg} = toml:term_2_binary({<<"r">>,
                                    [{<<"group_id">>,GroupId},
                                     {<<"t">>,<<"delete_group_member">>},
                                     {<<"id">>,<<"d_01">>}]}),
    gen_tcp:send(State#state.socket, Msg),
    io:format ("~p client send r!~n", [self()]),
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
    case H of
        {<<"rr">>, Attrs} ->
            case lists:keyfind(<<"id">>, 1, Attrs) of
                {<<"id">>, <<"abc_01">>} ->
                    case lists:keyfind(<<"s">>, 1, Attrs) of
                        {<<"s">>, 0} ->
                            io:format ("~p Login success, id is ~p~n", [self(), User#user.id]),
                            {ok, MsgList} = get_offline_msg(User#user.token),
                            io:format("~p Got offline msg lists ~p~n", [self(), MsgList]);
                        {<<"s">>, 1} ->
                            {<<"r">>, Reason} = lists:keyfind(<<"r">>, 1, Attrs),
                            io:format ("~p Login failed, reason is ~p~n", [self(), Reason]);
                        _ ->
                            io:format ("Login Error~n")
                    end;
                _ ->
                    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
                    Ack = <<"[[a]] id=\"", MsgId/binary, "\"">>,
                    io:format ("===Send ack: ~p~n", [Ack]),
                    gen_tcp:send(Socket, Ack),
                    io:format("~p Unkown response~n", [self()]),
                    ok
            end;
        {<<"m">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\"">>,
            io:format ("~p Send ack: ~p~n", [self(), Ack]),
            gen_tcp:send(Socket, Ack);
        {<<"gm">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\"">>,
            io:format ("~p Send ack: ~p~n", [self(), Ack]),
            gen_tcp:send(Socket, Ack);
        {<<"a">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            {ok, AckBin} = toml:term_2_binary({<<"a">>, Attrs}),
            % send ack back
            gen_tcp:send(Socket, AckBin),
            io:format ("~p Msg id=~p send success~n", [self(), MsgId]);
        {<<"r">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\"">>,
            io:format ("===Send ack: ~p~n", [Ack]),
            gen_tcp:send(Socket, Ack),
            io:format("~p Got request: ~p~n", [self(), {<<"r">>, Attrs}]);
        _ ->
            io:format("~p Unkown message: ~p~n", [self(), H]),
            ok
    end,
    process_package(T, State);
process_package([], _) ->
    ok.