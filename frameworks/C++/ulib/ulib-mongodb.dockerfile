FROM techempower/ulib-base:0.1

# 1. Change ULib Server (userver_tcp) configuration
RUN sed -i "s|TCP_LINGER_SET .*|TCP_LINGER_SET 0|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 256|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|CLIENT_FOR_PARALLELIZATION .*|CLIENT_FOR_PARALLELIZATION 100|g"  $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
ENV MONGODB_HOST=tfb-database
ENV UMEMPOOL="96,0,0,47,16401,-14,-20,-18,26"

CMD sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD $(( 3 * $(nproc) / 2 ))|g"  $IROOT/ULib/benchmark.cfg && \
    setcap cap_sys_nice,cap_sys_resource,cap_net_bind_service,cap_net_raw+eip  $IROOT/ULib/bin/userver_tcp && \
    $IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg
