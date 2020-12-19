FROM postgres:latest

RUN mkdir -p /docker-entrypoint-initdb.d
COPY src/*.sql /docker-entrypoint-initdb.d/
