FROM tfb/cutelyst-framework:latest

ENV TROOT=/cutelyst-benchmark-app
ENV LD_LIBRARY_PATH=${CMAKE_PREFIX_PATH}/lib
ENV CUTELYST_APP=${TROOT}/build/libcutelyst_benchmarks.so
    
ADD src ${TROOT}/
ADD config/config.ini /cutelyst.ini
ADD config/config_socket.ini /cutelyst_socket.ini

RUN sed -i "s|DatabaseHostName=.*|DatabaseHostName=${DBHOST}|g" /cutelyst.ini
RUN sed -i "s|DatabaseHostName=.*|DatabaseHostName=${DBHOST}|g" /cutelyst_socket.ini

RUN cd ${TROOT} && \
    mkdir -p build && \
    cd build && \
    cmake ${TROOT} \
    -DCMAKE_BUILD_TYPE=Release && \
    make
