FROM postgrest/postgrest:latest

FROM alpine
RUN apk add postgresql-client bash

COPY --from=0 /usr/local/bin/postgrest /usr/local/bin/postgrest
COPY --from=0 /etc/postgrest.conf /etc/postgrest.conf

ENV PGRST_DB_SCHEMA=public
ENV PGRST_DB_ANON_ROLE=
ENV PGRST_DB_POOL=100
ENV PGRST_DB_POOL_TIMEOUT=10
ENV PGRST_DB_EXTRA_SEARCH_PATH=public
ENV PGRST_DB_CHANNEL=pgrst
ENV PGRST_DB_CHANNEL_ENABLED=false
ENV PGRST_SERVER_HOST=*4
ENV PGRST_SERVER_PORT=3000
ENV PGRST_OPENAPI_SERVER_PROXY_URI=
ENV PGRST_JWT_SECRET=
ENV PGRST_SECRET_IS_BASE64=false
ENV PGRST_JWT_AUD=
ENV PGRST_MAX_ROWS=
ENV PGRST_PRE_REQUEST=
ENV PGRST_ROLE_CLAIM_KEY=.role
ENV PGRST_ROOT_SPEC=
ENV PGRST_RAW_MEDIA_TYPES=

ENV PGRST_DB_URI=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world
ENV PGRST_DB_SCHEMA=public
ENV PGRST_DB_ANON_ROLE=benchmarkdbuser
ENV PGRST_RAW_MEDIA_TYPES="text/html, text/plain"
ENV PGRST_DB_POOL=64
RUN mkdir /app
COPY src /app
RUN chmod +x /app/entrypoint.sh
WORKDIR /app

EXPOSE 3000

ENTRYPOINT [ "/app/entrypoint.sh" ]