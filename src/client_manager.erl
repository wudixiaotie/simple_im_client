-module(client_manager).

-behaviour(gen_server).

% APIs
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-include ("user.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    inets:start(),
    ssl:start(),
    ok = start_client(1),
    {ok, []}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

start_client(0) ->
    ok;
start_client(N) ->
    {ok, [{_, _, Phone, _}]} = users:find({id, N}),
    User = #user{phone = Phone, password = <<"888888">>, device = <<"android">>},
    {ok, Pid} = supervisor:start_child(client_sup, [User]),
    timer:sleep(20),
    start_client(N - 1).