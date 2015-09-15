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
    User1 = #user{phone = <<"18501260698">>, password = <<"888888">>, device = <<"android">>},
    User2 = #user{phone = <<"13833665577">>, password = <<"888888">>, device = <<"ipad">>},
    {ok, Pid1} = supervisor:start_child(client_sup, [User1]),
    {ok, Pid2} = supervisor:start_child(client_sup, [User2]),
    % gen_server:call(Pid1, send_msg),
    {ok, []}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================