#!/bin/sh
exec erl \
     +K true +sbwt very_long +swt very_low \
     -pa ebin deps/*/ebin \
     -boot start_sasl \
     -config priv/app.config \
     -s misultin_bench_app
