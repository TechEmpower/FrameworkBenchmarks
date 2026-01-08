FROM mongo:8.0

ENV MONGO_INITDB_DATABASE=hello_world
ENV MONGO_TCMALLOC_PER_CPU_CACHE_SIZE_BYTES=16777216

COPY create.js /docker-entrypoint-initdb.d/
