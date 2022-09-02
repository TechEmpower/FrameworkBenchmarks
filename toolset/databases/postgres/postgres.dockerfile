FROM buildpack-deps:bionic

ADD postgresql.conf postgresql.conf
ADD pg_hba.conf pg_hba.conf
ADD 60-postgresql-shm.conf 60-postgresql-shm.conf
ADD create-postgres-database.sql create-postgres-database.sql
ADD create-postgres.sql create-postgres.sql
ADD pgdg.list pgdg.list

# prepare PostgreSQL APT repository
RUN cp pgdg.list /etc/apt/sources.list.d/
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -

RUN apt-get -yqq update > /dev/null
RUN apt-get -yqq install locales

ENV PG_VERSION 14
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

# install postgresql on database machine
RUN apt-get -yqq install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" postgresql-${PG_VERSION} postgresql-contrib-${PG_VERSION}

# Make sure all the configuration files in main belong to postgres
RUN sed -i "s|PG_VERSION|${PG_VERSION}|g" postgresql.conf 
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

ENV PATH $PATH:/usr/lib/postgresql/$PG_VERSION/bin

CMD ["postgres"]
