FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -qqy locales

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8 

ADD postgresql.conf postgresql.conf
ADD pg_hba.conf pg_hba.conf
ADD 60-postgresql-shm.conf 60-postgresql-shm.conf
ADD create-postgres-database.sql create-postgres-database.sql
ADD create-postgres.sql create-postgres.sql
ADD wait.sh wait.sh

# install postgresql on database machine
RUN apt-get -y update
RUN apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" postgresql

ENV PG_VERSION 9.5

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

USER postgres

# We have to wait for postgres to start before we can use the cli
RUN service postgresql start && \
    until psql -c "\q"; do sleep 1; done && \
    psql < create-postgres-database.sql && \
    psql -q hello_world < create-postgres.sql

EXPOSE 5432
ENV PATH $PATH:/usr/lib/postgresql/$PG_VERSION/bin
ENV PGDATA=/var/lib/postgresql/data 

CMD ["postgres", "-D", "/ssd/postgresql"]