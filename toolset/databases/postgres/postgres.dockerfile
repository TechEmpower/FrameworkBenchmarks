FROM postgres:18-trixie

ENV PGDATA=/ssd/postgresql \
    POSTGRES_DB=hello_world \
    POSTGRES_PASSWORD=benchmarkdbpass \
    POSTGRES_USER=benchmarkdbuser

COPY 60-postgresql-shm.conf /etc/sysctl.d/
COPY config.sh create-postgres.sql /docker-entrypoint-initdb.d/
COPY postgresql.conf /tmp/
