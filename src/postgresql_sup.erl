%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% postgresql client supervisor
%% ===================================================================

-module(postgresql_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Args), #{id       => postgresql_worker,
                       start    => {postgresql_worker, start_link, Args},
                       restart  => permanent,
                       type     => worker}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DbHost = "localhost",
    DbUsername = "postgres",
    DbPassword = "postgres",
    DbDatabase = "test",
    DbPort = 5432,
    Spec = ?CHILD([DbHost, DbUsername, DbPassword, [
        {database, DbDatabase},
        {port, DbPort},
        {timeout, 4000}
    ]]),

    {ok, { {simple_one_for_one, 10, 5}, [Spec] } }.