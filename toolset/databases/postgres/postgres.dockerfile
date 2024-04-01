FROM postgres:16-bookworm

ENV PGDATA=/ssd/postgresql \
    POSTGRES_DB=hello_world \
    POSTGRES_HOST_AUTH_METHOD=md5 \
    POSTGRES_INITDB_ARGS=--auth-host=md5 \
    POSTGRES_PASSWORD=benchmarkdbpass \
    POSTGRES_USER=benchmarkdbuser

COPY postgresql.conf /tmp/

COPY config.sh create-postgres.sql /docker-entrypoint-initdb.d/

COPY 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf
