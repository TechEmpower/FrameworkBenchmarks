FROM postgrest/postgrest:latest

ENV PGRST_DB_URI=postgres://tfb:password@localhost/tfb
ENV PGRST_DB_SCHEMA=public
ENV PGRST_DB_ANON_ROLE=tfb
WORKDIR /postgrest
