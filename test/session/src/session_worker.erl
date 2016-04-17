-module(session_worker).

-export([start_link/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).


start_link(Socket) ->
    gen_msg:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, list]),
    {ok, []}.
handle_msg({tcp, Socket, [$f|UserIdStr]}, State) ->
    UserId = erlang:list_to_integer(UserIdStr),

    Result = case ets:lookup(session, UserId) of
        [] ->
            "offline";
        [{UserId, Pid}] ->
            Pid
    end,
    ok = gen_tcp:send(Socket, Result),
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(_Info, State) -> {ok, State}.

terminate(Reason, _State) ->
    io:format("terminate:~p~n", [Reason]),
    ok.