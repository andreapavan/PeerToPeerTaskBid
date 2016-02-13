#! /bin/sh

erl -sname $1 -pa riak-erlang-client/ebin riak-erlang-client/deps/*/ebin -extra $2 $3
