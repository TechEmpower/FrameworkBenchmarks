FROM ubuntu:24.04

# Required scripts for benchmarking
COPY concurrency.sh pipeline.lua pipeline.sh query.sh ./

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get install --no-install-recommends -qqUy curl wrk > /dev/null

# Environment vars required by the wrk scripts with nonsense defaults
ENV accept=accept \
    duration=duration \
    levels=levels \
    max_concurrency=max_concurrency \
    max_threads=max_threads \
    name=name \
    pipeline=pipeline \
    server_host=server_host
