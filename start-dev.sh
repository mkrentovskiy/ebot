#!/bin/sh
erl +A 4 -pa ebin edit deps/*/ebin -boot start_sasl -sname ebot -s ebot -s reloader
