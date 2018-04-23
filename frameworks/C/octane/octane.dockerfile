FROM ubuntu:16.04

ADD ./ /octane
WORKDIR /octane

RUN apt update -yqq && \
    apt install -yqq software-properties-common build-essential git cmake automake libtool wget

# May 22nd, 2017
RUN git clone https://github.com/simongui/octane.git && \
    cd octane && \
    git checkout 8c28b1b83f1aa2817a401a3e8437a0af4ec53c28 && \
    make

ENV PATH=/octane/octane/build/:${PATH}
ENV LD_PRELOAD=/octane/octane/lib/lockless_allocator/libllalloc.so.1.3

CMD ["techempower_benchmarks"]
