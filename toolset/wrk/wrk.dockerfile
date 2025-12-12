FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get install --no-install-recommends -qqUy curl wrk > /dev/null

# Required scripts for benchmarking
COPY concurrency.sh pipeline.lua pipeline.sh query.sh ./
