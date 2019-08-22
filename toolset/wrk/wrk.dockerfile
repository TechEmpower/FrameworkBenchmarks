FROM buildpack-deps:bionic

WORKDIR /wrk
RUN curl -sL https://github.com/wg/wrk/archive/4.1.0.tar.gz | tar xz --strip-components=1
RUN make > /dev/null
RUN cp wrk /usr/local/bin

WORKDIR /
# Required scripts for benchmarking
COPY pipeline.lua pipeline.lua
COPY concurrency.sh concurrency.sh
COPY pipeline.sh pipeline.sh
COPY query.sh query.sh

RUN chmod 777 pipeline.lua concurrency.sh pipeline.sh query.sh

# Environment vars required by the wrk scripts with nonsense defaults
ENV name name
ENV server_host server_host
ENV levels levels
ENV duration duration
ENV max_concurrency max_concurrency
ENV max_threads max_threads
ENV pipeline pipeline
ENV accept accept
