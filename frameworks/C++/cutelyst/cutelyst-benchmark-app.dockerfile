FROM tfb/cutelyst-framework:latest

ENV TROOT=/cutelyst-benchmark-app
ENV LD_LIBRARY_PATH=${CMAKE_PREFIX_PATH}/lib
ENV CUTELYST_APP=${TROOT}/build/src/libcutelyst_benchmarks.so
    
ADD CMakeLists.txt ${TROOT}/
ADD src ${TROOT}/src
ADD config/config.ini /cutelyst.ini
ADD config/config_socket.ini /cutelyst_socket.ini

RUN cd ${TROOT} && \
    mkdir -p build && \
    cd build && \
    cmake ${TROOT} \
    -DCMAKE_BUILD_TYPE=Release && \
    make -j ${CPU_COUNT}
