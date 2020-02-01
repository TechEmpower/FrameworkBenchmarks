FROM buildpack-deps:focal

RUN apt update -yqq
RUN apt install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev g++-9 gdb

COPY ./ ./

ENV COMMIT=c63fddba232e490e151d2446e9ea20b189b15693

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -g -DTFB_MYSQL -O3 -march=native -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -o /lithium_tbf

CMD gdb -batch -x ./gdb.cmds --args /lithium_tbf tfb-database 8080
