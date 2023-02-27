FROM ubuntu:22.04

# Required scripts for benchmarking
COPY concurrency.sh pipeline.lua pipeline.sh query.sh ./

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update >/dev/null && \
    apt-get -yqq install >/dev/null \
      curl \
      wrk && \
    chmod 777 concurrency.sh pipeline.sh query.sh

# Environment vars required by the wrk scripts with nonsense defaults
ENV accept=accept \
    duration=duration \
    levels=levels \
    max_concurrency=max_concurrency \
    max_threads=max_threads \
    name=name \
    pipeline=pipeline \
    server_host=server_host
