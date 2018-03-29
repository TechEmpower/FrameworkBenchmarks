FROM tfb/ulib-base:latest

# 1. Change ULib Server (userver_tcp) configuration
RUN sed -i "s|TCP_LINGER_SET .*|TCP_LINGER_SET -2|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 256|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|CLIENT_FOR_PARALLELIZATION .*|CLIENT_FOR_PARALLELIZATION 100|g"  $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
ENV ORM_DRIVER="pgsql"
ENV UMEMPOOL="581,0,0,59,16409,-7,-20,-23,31"
ENV ORM_OPTION="host=TFB-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world client_encoding=UTF8"

CMD sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD $(( 2 * $(nproc)))|g"  $IROOT/ULib/benchmark.cfg && \
    setcap cap_sys_nice,cap_sys_resource,cap_net_bind_service,cap_net_raw+eip  $IROOT/ULib/bin/userver_tcp && \
    $IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg
