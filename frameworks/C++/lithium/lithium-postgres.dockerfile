FROM buildpack-deps:focal

RUN apt update -yqq
RUN apt install -yqq g++-9 libboost-dev postgresql-server-dev-all libpq-dev wget libboost-context-dev

COPY ./ ./

ENV COMMIT=c63fddba232e490e151d2446e9ea20b189b15693

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_pgsql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -DTFB_PGSQL -O3 -DNDEBUG -march=native -std=c++17 ./lithium.cc -I/usr/include/postgresql -lpthread -lpq -lboost_context -o /lithium_tbf

CMD /lithium_tbf tfb-database 8080
