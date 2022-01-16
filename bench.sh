#!/usr/bin/env bash

rebar3 as bench compile
ERL_LIBS=_build/bench/lib escript bench.erl
