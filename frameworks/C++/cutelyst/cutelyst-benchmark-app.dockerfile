FROM tfb/cutelyst-framework:latest

ENV TROOT=/cutelyst-benchmark-app
    
ADD ./ ${TROOT}

RUN ldd /usr/bin/cutelyst-wsgi2

RUN cd ${TROOT} && mkdir build && cd build && \
    cmake .. \
    -DCMAKE_BUILD_TYPE=Release && \
    make -j ${CPU_COUNT}
