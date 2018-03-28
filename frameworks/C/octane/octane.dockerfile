FROM techempower/base:0.1

COPY ./ ./

RUN apt install -yqq cmake automake libtool

# May 22nd, 2017
RUN git clone https://github.com/simongui/octane.git && \
    cd octane && \
    git checkout 8c28b1b83f1aa2817a401a3e8437a0af4ec53c28 && \
    make

ENV PATH=octane/build/:${PATH}
ENV LD_PRELOAD=octane/lib/lockless_allocator/libllalloc.so.1.3

CMD ["techempower_benchmarks"]
