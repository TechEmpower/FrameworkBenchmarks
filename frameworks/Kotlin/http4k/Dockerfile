FROM postgres:9.6.0

ADD sql /docker-entrypoint-initdb.d/
EXPOSE 5432
ENV POSTGRES_DB hello_world
ENV POSTGRES_USER benchmarkdbuser
ENV POSTGRES_PASSWORD benchmarkdbpass
