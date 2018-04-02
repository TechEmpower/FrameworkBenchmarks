FROM techempower/ulib-base:0.1

# 1. Change ULib Server (userver_tcp) configuration
RUN sed -i "s|TCP_LINGER_SET .*|TCP_LINGER_SET 0|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 16384|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|CLIENT_FOR_PARALLELIZATION .*|CLIENT_FOR_PARALLELIZATION 8000|g"  $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
ENV UMEMPOOL="58,0,0,41,16401,-14,-15,11,25"

CMD sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD $(nproc)|g"  $IROOT/ULib/benchmark.cfg && \
    setcap cap_sys_nice,cap_sys_resource,cap_net_bind_service,cap_net_raw+eip  $IROOT/ULib/bin/userver_tcp && \
    $IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg
