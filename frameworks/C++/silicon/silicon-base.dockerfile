FROM techempower/microhttpd:0.1

RUN apt install -yqq libboost-dev cmake

ENV SILICON=/silicon

COPY ./ ./

RUN git clone https://github.com/matt-42/silicon.git && \
    cd silicon && \
    git checkout ecaf04887c9dbbf0f457afab1f487268f6aeffab && \
    CC=clang-3.9 CXX=clang++-3.9 ./install.sh /
