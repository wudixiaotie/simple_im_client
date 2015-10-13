-module(client).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {socket, user}).

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
                  {"http://localhost:8080/server/login", [],
                   "application/x-www-form-urlencoded",
                   <<"phone=", (User#user.phone)/binary, "&password=", Password/binary>>},
                  [], []),
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
                    {ok, Socket} = gen_tcp:connect (Server, Port, [{packet,0}, {active, true}]),
                    State = #state{socket = Socket, user = NewUser},
                    Msg = <<"[[r]] id = \"abc_01\" t = \"login\" [r.user] id = \"", UserIdBin/binary,
                            "\" device = \"", (NewUser#user.device)/binary,
                            "\" token = \"", Token/binary, "\"">>,
                    gen_tcp:send(State#state.socket, Msg),
                    io:format ("===client login!~n"),
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

handle_info ({tcp, Socket, Data}, #state{socket = Socket, user = User} = State) ->
    io:format ("~p===Got msg: ~p~n", [self(), Data]),
    {ok, Toml} = toml:binary_2_term(Data),
    case Toml of
        [{<<"rr">>, Attrs}] ->
            case lists:keyfind(<<"t">>, 1, Attrs) of
                {<<"t">>, <<"login">>} ->
                    case lists:keyfind(<<"s">>, 1, Attrs) of
                        {<<"s">>, 0} ->
                            io:format ("Login success, id is ~p~n", [User#user.id]),
                            {ok, MsgList} = get_offline_msg(User#user.token),
                            io:format("==Got offline msg lists ~p~n", [MsgList]);
                        {<<"s">>, 1} ->
                            {<<"r">>, Reason} = lists:keyfind(<<"r">>, 1, Attrs),
                            io:format ("Login failed, reason is ~p~n", [Reason]);
                        _ ->
                            io:format ("Login Error~n")
                    end;
                {<<"t">>, <<"reconnect">>} ->
                    case lists:keyfind(<<"s">>, 1, Attrs) of
                        {<<"s">>, 0} ->
                            io:format ("reconnect success~n", []);
                        {<<"s">>, 1} ->
                            {<<"r">>, Reason} = lists:keyfind(<<"r">>, 1, Attrs),
                            io:format ("reconnect failed, reason is ~p~n", [Reason]);
                        _ ->
                            io:format ("Login Error~n")
                    end
            end;
        [{<<"m">>, Attrs}] ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[[a]] id=\"", MsgId/binary, "\"">>,
            io:format ("===Send ack: ~p~n", [Ack]),
            gen_tcp:send(Socket, Ack);
        [{<<"a">>, Attrs}] ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            % send ack back
            gen_tcp:send(Socket, Data),
            io:format ("===Msg id=~p send success~n", [MsgId])
    end,
    {noreply, State};
handle_info ({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, tcp_closed, State};
handle_info(send_msg, #state{user = User} = State) ->
    UserIdBin = erlang:integer_to_binary(User#user.id),
    Msg = <<"[[m]] id = \"a_02\" c = \"hello\" [m.from] id = ", UserIdBin/binary, " device = \"", (User#user.device)/binary, "\" [m.to] id = 3">>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("===client send msg!~n"),
    {noreply, State};
handle_info(send_group_msg, #state{user = User} = State) ->
    UserIdBin = erlang:integer_to_binary(User#user.id),
    Msg = <<"[[gm]] id = \"a_03\" c = \"hello\" [gm.user] id = ", UserIdBin/binary, " device = \"", (User#user.device)/binary, "\" [gm.group] id = 3">>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("===client send msg!~n"),
    {noreply, State};
handle_info(reconnect, #state{user = User} = State) ->
    UserIdBin = erlang:integer_to_binary(User#user.id),
    Token = uri_encode(User#user.token),
    Result = httpc:request(post,
                  {"http://localhost:8080/server/reconnect", [],
                   "application/x-www-form-urlencoded",
                   <<"id=", UserIdBin/binary, "&token=", Token/binary>>},
                  [], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            BodyBin = erlang:list_to_binary(Body),
            {ok, [{<<"response">>, Attrs}]} = toml:binary_2_term(BodyBin),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    {<<"server">>, ServerBin} = lists:keyfind(<<"server">>, 1, Attrs),
                    {<<"port">>, PortBin} = lists:keyfind(<<"port">>, 1, Attrs),
                    Server = erlang:binary_to_list(ServerBin),
                    Port = erlang:binary_to_integer(PortBin),
                    io:format("~p reconnect to ~p:~p~n", [self(), Server, Port]),
                    {ok, Socket} = gen_tcp:connect (Server, Port, [{packet,0}, {active, true}]),
                    % {ok, Socket} = gen_tcp:connect ("192.168.1.137", 1987, [{packet,0}, {active, true}]),
                    NewState = State#state{socket = Socket},
                    Msg = <<"[[r]] id = \"a_04\" t = \"reconnect\" [r.user] id = ", UserIdBin/binary,
                            " device = \"", (User#user.device)/binary,
                            "\" token = \"", (User#user.token)/binary, "\"">>,
                    gen_tcp:send(NewState#state.socket, Msg),
                    io:format ("===client reconnect! ~p~n", [Token]),
                    {noreply, NewState};
                _ ->
                   {stop, error, State}
            end;
        _ ->
            {stop, error, State}
    end;
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
    TokenEncode = uri_encode(Token),
    Result = httpc:request(post,
                  {"http://localhost:8080/offline", [],
                   "application/x-www-form-urlencoded",
                   <<"token=", (TokenEncode)/binary>>},
                  [], []),
    case Result of
        {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} ->
            {ok, Response} = toml:binary_2_term(Body),
            {<<"response">>,Attrs} = lists:keyfind(<<"response">>, 1, Response),
            case lists:keyfind(<<"status">>, 1, Attrs) of
                {<<"status">>, 0} ->
                    MsgList = lists:keydelete(<<"response">>, 1, Response),
                    {ok, MsgList};
                _ ->
                   {stop, http_request_failed}
            end;
        _ ->
            io:format("Can not connect to http server~n"),
            {stop, http_connect_failed}
    end.
