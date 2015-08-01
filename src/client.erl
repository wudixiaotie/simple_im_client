-module(client).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {socket}).

%% ===================================================================
%% APIs
%% ===================================================================

start_link(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([UserId]) ->
    {ok, Socket} = gen_tcp:connect ("localhost", 1987, [{packet,0}, {active, true}]),
    % {ok, Socket} = gen_tcp:connect ("192.168.1.137", 1987, [{packet,0}, {active, true}]),
    State = #state{socket = Socket},
    Msg = <<"[r] id=\"a_01\" c=\"login\" userid=\"", UserId/binary, "\"">>,
    gen_tcp:send(State#state.socket, Msg),
    io:format ("===client login!~n"),
    {ok, State}.


handle_call(send_msg, _From, State) ->
    Msg = <<"[m] id=\"a_02\" c=\"hello\" from=\"1@android\" to=\"2@ipad\"">>,
    Result = gen_tcp:send(State#state.socket, Msg),
    io:format ("===client send msg!~n"),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.


handle_cast(_Msg, State) -> {noreply, State}.

handle_info ({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    io:format ("===Got msg: ~p~n", [Data]),
    {ok, Toml} = etoml:parse(Data),
    case Toml of
        [{<<"rr">>, Attrs}] ->
            {<<"c">>, Content} = lists:keyfind(<<"c">>, 1, Attrs),
            io:format ("Login ~p~n", [Content]);
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