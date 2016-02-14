#! /bin/sh

erl -name $1 -setcookie $4 -pa riak-erlang-client/ebin riak-erlang-client/deps/*/ebin -extra $2 $3
