FROM postgres:18-trixie

ENV POSTGRES_DB=hello_world \
    POSTGRES_PASSWORD=benchmarkdbpass \
    POSTGRES_USER=benchmarkdbuser

COPY 60-postgresql-shm.conf /etc/sysctl.d/
COPY config.sh create-postgres.sql postgresql.conf /docker-entrypoint-initdb.d/
