FROM ubuntu:22.04

ARG PG_VERSION=15

COPY 60-postgresql-shm.conf pg_hba.conf postgresql.conf ./
COPY create-postgres.sql create-postgres-database.sql /tmp/

# Prepare the PostgreSQL APT repository
ARG DEBIAN_FRONTEND=noninteractive
ADD "https://www.postgresql.org/media/keys/ACCC4CF8.asc" /etc/apt/keyrings/postgresql.asc
RUN chmod 644 /etc/apt/keyrings/postgresql.asc && \
    apt-get -yqq update && \
    apt-get -yqq install \
      apt-utils \
      locales \
      lsb-release && \
    echo "deb [ signed-by=/etc/apt/keyrings/postgresql.asc ] http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > \
      /etc/apt/sources.list.d/pgdg.list && \
    locale-gen en_US.UTF-8

ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Install PostgreSQL on the database machine
RUN apt-get -yqq update && \
    apt-get -yqq install \
      -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
      postgresql-${PG_VERSION} \
      postgresql-contrib-${PG_VERSION} && \
    sed -i "s|PG_VERSION|${PG_VERSION}|g" postgresql.conf && \
    mv pg_hba.conf postgresql.conf /etc/postgresql/${PG_VERSION}/main && \
    mkdir /ssd && \
    cp -Rp /var/lib/postgresql/${PG_VERSION}/main /ssd/postgresql && \
    cp /etc/postgresql/${PG_VERSION}/main/postgresql.conf /ssd/postgresql && \
    mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf && \
    chown -Rf postgres:postgres \
      /etc/postgresql/${PG_VERSION}/main \
      /etc/sysctl.d/60-postgresql-shm.conf \
      /ssd \
      /tmp/create-postgres* \
      /var/run/postgresql && \
    chmod 2777 /var/run/postgresql

ENV PGDATA=/ssd/postgresql

USER postgres

# We have to wait for PostgreSQL to start before we can use the CLI
RUN service postgresql start && \
    until psql -c "\q"; do sleep 1; done && \
    psql < /tmp/create-postgres-database.sql && \
    psql -a hello_world < /tmp/create-postgres.sql && \
    service postgresql stop

ENV PATH=${PATH}:/usr/lib/postgresql/${PG_VERSION}/bin

CMD ["postgres"]
