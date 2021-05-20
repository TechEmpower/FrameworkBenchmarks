FROM sumeetchhetri/ffead-cpp-sql-raw-profiled-base:6.0
LABEL maintainer="Sumeet Chhetri"
LABEL version="6.0"
LABEL description="SQL Raw Base ffead-cpp-v-picov-profiled docker image with commit id - master"

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0-sql/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so && \
	ln -s ${IROOT}/ffead-cpp-6.0-sql/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-6.0-sql/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-6.0-sql/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0-sql/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

RUN apt update -yqq && apt install -y git make && rm -rf /var/lib/apt/lists/*
#For Latest vlang, uncomment the below mentioned line, due to lot of new enhancements and unsafe block handling, vlnag has slowed down tremendously
#RUN git clone https://github.com/vlang/v && cd v && make && ./v symlink

#For the fastest vlang performance, use 0.1.29, where the unsafe changes were only restricted to pointer arithmetic
RUN wget -q https://github.com/vlang/v/releases/download/0.1.29/v_linux.zip && unzip -q v_linux.zip && cd v && chmod +x v && ./v symlink && cd .. && rm -f v_linux.zip

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

COPY sql-v-picov-profiled-install.sh install_ffead-cpp-sql-raw-v-picov-profiled.sh ${IROOT}/
RUN chmod 755 ${IROOT}/sql-v-picov-profiled-install.sh ${IROOT}/install_ffead-cpp-sql-raw-v-picov-profiled.sh
RUN ./sql-v-picov-profiled-install.sh
