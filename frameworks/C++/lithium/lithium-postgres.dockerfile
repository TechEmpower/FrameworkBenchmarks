FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq g++-9 libboost-dev postgresql-server-dev-all libpq-dev wget libboost-context-dev

COPY ./ ./

ENV COMMIT=c4ba8bed7fc4dff8604cb19fdf97188ec330b2a0

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_pgsql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -DTFB_PGSQL -O3 -DNDEBUG -march=native -std=c++17 ./lithium.cc -I/usr/include/postgresql -I /usr/include/postgresql/12/server -lpthread -lpq -lboost_context -lssl -lcrypto -o /lithium_tbf

CMD /lithium_tbf tfb-database 8080
