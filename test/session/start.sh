#!/bin/sh
rebar3 compile

erl_start="erl -pa _build/default/lib/*/ebin -smp +Q 250000 +P 250000 +K true"

case $1 in
    "o" )
        $erl_start -eval "observer:start ()."
        ;;
    "a" )
        $erl_start -eval "application:start(session)."
        ;;
    "oa" | "ao" )
        $erl_start -eval "observer:start ()." -eval "application:start(session)."
        ;;
    "" )
        $erl_start
        ;;
    * )
        echo "unknown args!"
        ;;
esac