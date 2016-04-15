%%%-------------------------------------------------------------------
%% @doc session top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(M, T), {M, {M, start_link, []}, permanent, 5000, T, [M]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [?CHILD(session_worker_sup, supervisor),
                                 ?CHILD(session, worker)]} }.

%%====================================================================
%% Internal functions
%%====================================================================
