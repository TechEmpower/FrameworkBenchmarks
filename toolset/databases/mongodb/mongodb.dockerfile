FROM mongo:7.0

ENV MONGO_INITDB_DATABASE=hello_world

COPY create.js /docker-entrypoint-initdb.d/
