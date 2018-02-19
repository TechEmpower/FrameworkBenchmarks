FROM tfb:latest

COPY ./ ./

ENV ONION_LOG noinfo

# Latest commit on master as of July 10 2014
# This is post tag v0.7, but pre any later tags

RUN git clone https://github.com/davidmoreno/onion.git && \
    cd onion && \
    git checkout c460557bfc7d45fb6ba61cb6b7259480a67dde82 && \
    mkdir -p build && \
    cd build && \
    cmake .. && \
    make && \
    make clean && \
    make

CMD ["./onion/build/hello"]
