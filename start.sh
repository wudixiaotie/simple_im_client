#!/bin/sh
rebar compile

erl_start="erl -pa ebin/ -pa deps/*/ebin/ -smp +Q 250000 +P 250000 +K true"

case $1 in
    "o" )
        $erl_start -eval "observer:start ()."
        ;;
    "a" )
        $erl_start -eval "application:start(simple_im_client)."
        ;;
    "oa" | "ao" )
        $erl_start -eval "observer:start ()." -eval "application:start(simple_im_client)."
        ;;
    "" )
        $erl_start
        ;;
    * )
        echo "unknown args!"
        ;;
esac