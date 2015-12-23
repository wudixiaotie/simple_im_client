%% ===================================================================
%% Author xiaotie
%% 2015-11-06
%% database tasks
%% ===================================================================

-module (db).

-export ([create_test_users/2]).
-compile (export_all).



%% ===================================================================
%% APIs
%% ===================================================================

create_test_users(Begin, End) ->
    Start = 8613900000000,
    create_test_users(Start, Begin, End).
create_test_users(_, End, End) ->
    ok;
create_test_users(Start, Current, End) ->
    CurrentBin = erlang:integer_to_binary(Current),
    Mobile = erlang:integer_to_binary(Start + Current),
    users:create(<<"test_", CurrentBin/binary>>, Mobile, <<"888888">>),
    create_test_users(Start, Current + 1, End).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================