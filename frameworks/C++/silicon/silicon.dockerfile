FROM techempower/silicon-base:0.1

RUN cd build && \
    cmake .. -DCMAKE_CXX_COMPILER=clang++-3.9 && \
    make silicon_tpc_mysql

CMD /build/silicon_tpc_mysql ${DBHOST} 8080 ${CPU_COUNT}
