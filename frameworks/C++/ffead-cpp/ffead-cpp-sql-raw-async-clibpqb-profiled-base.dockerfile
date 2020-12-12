FROM sumeetchhetri/ffead-cpp-5.0-base:5.2
LABEL maintainer="Sumeet Chhetri"
LABEL version="5.2"
LABEL description="SQL Raw Custom libpq batch patched Base ffead-cpp docker image with commit id - master"

WORKDIR /tmp
RUN mkdir postgresql

COPY postgresql/* /tmp/postgresql/

#POSTGRESQL
WORKDIR /tmp/postgresql/

# prepare PostgreSQL APT repository
RUN apt-get -yqq update && apt-get -yqq install locales gnupg lsb-release

RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" | tee  /etc/apt/sources.list.d/pgdg.list

ENV PG_VERSION 13
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

# install postgresql on database machine
RUN apt-get -yqq update && apt-get -yqq install postgresql-13 postgresql-contrib-13 &&  rm -rf /var/lib/apt/lists/*

# Make sure all the configuration files in main belong to postgres
RUN mv postgresql.conf /etc/postgresql/${PG_VERSION}/main/postgresql.conf
RUN mv pg_hba.conf /etc/postgresql/${PG_VERSION}/main/pg_hba.conf

RUN chown -Rf postgres:postgres /etc/postgresql/${PG_VERSION}/main

RUN mkdir /ssd
RUN cp -R -p /var/lib/postgresql/${PG_VERSION}/main /ssd/postgresql
RUN cp /etc/postgresql/${PG_VERSION}/main/postgresql.conf /ssd/postgresql
RUN mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

RUN chown -Rf postgres:postgres /var/run/postgresql
RUN chmod 2777 /var/run/postgresql
RUN chown postgres:postgres /etc/sysctl.d/60-postgresql-shm.conf
RUN chown postgres:postgres create-postgres*
RUN chown -Rf postgres:postgres /ssd

ENV PGDATA=/ssd/postgresql

USER postgres

# We have to wait for postgres to start before we can use the cli
RUN service postgresql start && \
    until psql -c "\q"; do sleep 1; done && \
    psql < create-postgres-database.sql && \
    psql -a hello_world < create-postgres.sql && \
    service postgresql stop
#POSTGRESQL

USER root

#WRK
WORKDIR /tmp/wrk
RUN apt-get -yqq update && apt-get -yqq install libluajit-5.1-dev libssl-dev luajit && rm -rf /var/lib/apt/lists/* && \
	curl -sL https://github.com/wg/wrk/archive/4.1.0.tar.gz | tar xz --strip-components=1
ENV LDFLAGS="-O3 -march=native -flto"
ENV CFLAGS="-I /usr/include/luajit-2.1 $LDFLAGS"
RUN make WITH_LUAJIT=/usr WITH_OPENSSL=/usr -j "$(nproc)"
RUN cp wrk /usr/local/bin

ENV name name
ENV server_host server_host
ENV levels levels
ENV duration duration
ENV max_concurrency max_concurrency
ENV max_threads max_threads
ENV pipeline pipeline
ENV accept accept
#WRK

WORKDIR ${IROOT}

COPY sql-async-profiled-install-clang.sh install_ffead-cpp-sql-raw-profiled.sh ${IROOT}/
RUN chmod 755 ${IROOT}/sql-async-profiled-install-clang.sh ${IROOT}/install_ffead-cpp-sql-raw-profiled.sh

RUN ./sql-async-profiled-install-clang.sh batch
