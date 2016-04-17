-module(session_client).

-behaviour (gen_msg).

-export ([start/0, find/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

start() ->
    gen_msg:start_link(?MODULE, [], []).
find(Pid) ->
    Pid ! {find, self()},
    receive
        Result ->
            Result
    after
        1000 ->
            "timeout"
    end.

init([]) ->
    {ok, Sock} = gen_tcp:connect("localhost", 10001, [{active, true}, {packet, raw}, list]),
    gen_tcp:send(Sock, <<"Ready">>),
    {ok, Sock}.
handle_msg({find, From}, State) ->
    ok = gen_tcp:send(State, <<"f2">>),
    receive
        {tcp, Socket, Result} ->
            From ! Result
    after
        1000 ->
            io:format("client=============timeout~n")
    end,
    {ok, State};
handle_msg({tcp, Socket, A}, State) ->
    io:format("client=============~p~n", [A]),
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(_Info, State) -> {ok, State}.

terminate(Reason, _State) ->
    io:format("terminate:~p~n", [Reason]),
    ok.