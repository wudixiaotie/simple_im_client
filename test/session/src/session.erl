-module(session).

-export([start_link/0, init/0]).


start_link() ->
    Pid = spawn_opt(?MODULE, init, [], [link]),
    {ok, Pid}.


init() ->
    ets:new(session, [named_table,
                      public,
                      {read_concurrency, true},
                      {write_concurrency, true}]),
    ets:insert(session, {2, "<0,43,0>"}),
    true = erlang:register(?MODULE, self()),
    Opts = [binary,
            {packet, 0},
            {active, false}],
    {ok, Listen} = gen_tcp:listen(10001, Opts),
    accept(Listen).


accept(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    case inet:peername(Socket) of
        {ok, {ClientAddr, ClientPort}} ->
            io:format("[Session] Got a connect from: ~p(~p)~n", [ClientAddr, ClientPort]),

            ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
            receive
                {tcp, Socket, <<"Ready">>} ->
                    case supervisor:start_child(session_worker_sup, [Socket]) of
                        {ok, Pid} ->
                            ok = gen_tcp:controlling_process(Socket, Pid);
                        _ ->
                            io:format("[Session] worker start failed~n"),
                            ok = gen_tcp:close(Socket)
                    end
            after
                1000 ->
                    io:format("[Session] worker start timeout~n")
            end;
        _ ->
            ok
    end,
    accept(Listen).