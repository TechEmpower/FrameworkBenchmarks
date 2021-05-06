FROM debian:stretch-slim AS builder
RUN apt update
RUN apt install -y g++ curl make tar gzip libfindbin-libs-perl
RUN curl -L -o 0.0.2.tar.gz -L https://github.com/just-js/just/archive/0.0.2.tar.gz
RUN tar -zxvf 0.0.2.tar.gz
WORKDIR /just-0.0.2
RUN make runtime
RUN curl -L -o modules.tar.gz https://github.com/just-js/modules/archive/0.0.3.tar.gz
RUN tar -zxvf modules.tar.gz
RUN mv modules-0.0.3 modules
RUN JUST_HOME=$(pwd) make -C modules/picohttp/ deps http.so
RUN JUST_HOME=$(pwd) make -C modules/html/ html.so

FROM debian:stretch-slim
WORKDIR /app
RUN mkdir -p /app/lib
COPY lib/stringify.js lib/connection.js lib/dns.js lib/http.js lib/lookup.js lib/pg.js lib/stats.js lib/tcp.js lib/md5.js lib/monitor.js ./lib/
COPY techempower.js spawn.js ./
COPY --from=builder /just-0.0.2/just /bin/just
COPY --from=builder /just-0.0.2/modules/picohttp/http.so ./
COPY --from=builder /just-0.0.2/modules/html/html.so ./
ENV LD_LIBRARY_PATH=/app
ENV PGPOOL=1

EXPOSE 8080

CMD ["just", "spawn.js", "techempower.js"]
