FROM ubuntu:16.04

RUN apt-get update -yqq && \
    apt-get install -yqq software-properties-common build-essential git cmake automake libtool wget

WORKDIR /octane
RUN git clone https://github.com/simongui/octane.git .
RUN git checkout 8c28b1b83f1aa2817a401a3e8437a0af4ec53c28
RUN rm -rf src/techempower_benchmarks
COPY src src/techempower_benchmarks

RUN make

ENV LD_PRELOAD /octane/lib/lockless_allocator/libllalloc.so.1.3

EXPOSE 8000

CMD ["./build/techempower_benchmarks"]
