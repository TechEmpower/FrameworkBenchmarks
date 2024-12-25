FROM postgres:17-bookworm

ENV PGDATA=/ssd/postgresql \
    POSTGRES_DB=hello_world \
    POSTGRES_HOST_AUTH_METHOD=md5 \
    POSTGRES_INITDB_ARGS=--auth-host=md5 \
    POSTGRES_PASSWORD=benchmarkdbpass \
    POSTGRES_USER=benchmarkdbuser

COPY 60-postgresql-shm.conf /etc/sysctl.d/
COPY config.sh create-postgres.sql /docker-entrypoint-initdb.d/
COPY postgresql.conf /tmp/
