#! /bin/sh

cp $1/patches/riak_core.rebar.config $1/deps/riak_core/rebar.config
mkdir -p $1/deps/webmachine/include
mkdir -p $1/deps/webmachine/ebin
touch $1/deps/webmachine/include/webmachine.hrl
cp $1/patches/webmachine.app $1/deps/webmachine/ebin
cp $1/patches/riak_core.app $1/deps/riak_core/ebin
ERL_LIBS=$1/deps rebar compile deps_dir=$1/deps
rm -rf $1/deps/webmachine