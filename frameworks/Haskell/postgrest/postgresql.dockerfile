FROM postgres:latest

RUN mkdir -p /docker-entrypoint-initdb.d
COPY *.sql /docker-entrypoint-initdb.d/
ENV PGRST_DB_URI=postgres://tfb:password@localhost/tfb
ENV PGRST_DB_SCHEMA=public
ENV PGRST_DB_ANON_ROLE=anonymous
WORKDIR /postgrest
