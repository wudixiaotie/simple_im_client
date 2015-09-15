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
    {ok, Socket} = gen_tcp:connect ("localhost", 1987, [{packet,0}, {active, true}]),
    % {ok, Socket} = gen_tcp:connect ("192.168.1.137", 1987, [{packet,0}, {active, true}]),
    State = #state{socket = Socket, user = User},
    Msg = <<"[r] id = \"a_01\" c = \"login\" [r.user] phone = \"", (User#user.phone)/binary,
            "\" device = \"", (User#user.device)/binary,
            "\" password = \"", (User#user.password)/binary, "\"">>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("===client login!~n"),
    {ok, State}.


handle_call(send_msg, _From, #state{user = User} = State) ->
    Msg = <<"[m] id = \"a_02\" c = \"hello\" [m.from] id = ", (User#user.id)/binary, " device = \"", (User#user.device)/binary, "\" [m.to] id = 8 device = \"ipad\"">>,
    Result = gen_tcp:send(State#state.socket, Msg),
    io:format ("===client send msg!~n"),
    {reply, Result, State};
handle_call(send_group_msg, _From, #state{user = User} = State) ->
    Msg = <<"[gm] id = \"a_03\" c = \"hello\" [gm.user] id = ", (User#user.id)/binary, " device = \"", (User#user.device)/binary, "\" [gm.group] id = 3">>,
    Result = gen_tcp:send(State#state.socket, Msg),
    io:format ("===client send msg!~n"),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.


handle_cast(_Msg, State) -> {noreply, State}.

handle_info ({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    io:format ("~p===Got msg: ~p~n", [self(), Data]),
    {ok, Toml} = etoml:parse(Data),
    case Toml of
        [{<<"rr">>, Attrs}] ->
            case lists:keyfind(<<"s">>, 1, Attrs) of
                {<<"s">>, 0} ->
                    {<<"user_id">>, UserId} = lists:keyfind(<<"user_id">>, 1, Attrs),
                    io:format ("Login success, id is ~p~n", [UserId]);
                {<<"s">>, 1} ->
                    {<<"c">>, Reason} = lists:keyfind(<<"c">>, 1, Attrs),
                    io:format ("Login failed, reason is ~p~n", [Reason]);
                _ ->
                    io:format ("Login Error~n")
            end;
        [{<<"m">>, Attrs}] ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[a] id=\"", MsgId/binary, "\"">>,
            io:format ("===Send ack: ~p~n", [Ack]),
            gen_tcp:send(Socket, Ack);
        [{<<"a">>, Attrs}] ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            io:format ("===Msg id=~p send success~n", [MsgId])
    end,
    {noreply, State};
handle_info ({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================