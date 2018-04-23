FROM buildpack-deps:xenial

# wrk
RUN curl -sL -o wrk-4.0.1.tar.gz https://github.com/wg/wrk/archive/4.0.1.tar.gz
RUN tar xzf wrk-4.0.1.tar.gz
RUN cd wrk-4.0.1 && make
RUN cp wrk-4.0.1/wrk /usr/local/bin

# Required scripts for benchmarking
ADD pipeline.lua pipeline.lua
ADD concurrency.sh concurrency.sh
ADD pipeline.sh pipeline.sh
ADD query.sh query.sh

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
