-module(client).

-behaviour(gen_server).

% APIs
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {socket}).

%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, Socket} = gen_tcp:connect (<<"localhost">>, 1987, [{packet,0}, {active, true}]),
    State = #state{socket = Socket},
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.

% <<"[msg] id=\"a_01\" mc=\"hello\" from=\"1@android\" to=\"2@ipad\"">>
handle_info ({tcp, _Socket, Data}, #state{socket = Socket} = State) ->
    {ok, Toml} = etoml:parse(Data),
    [{<<"msg">>, Attrs}] = Toml,
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    gen_tcp:send(Socket, <<"[ack] id=\"", MsgId/binary, "\"">>),
    {noreply, State};
handle_info ({tcp_closed, _Socket}, State) ->
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================