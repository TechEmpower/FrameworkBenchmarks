FROM buildpack-deps:focal

RUN apt update -yqq
RUN apt install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev g++-9 gdb

COPY ./ ./

ENV COMMIT=d66c2fcf52606587601032124cf8a08d47aebaed

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -O3 -g -DTFB_MYSQL -DPLAINTEXT_ONLY -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -o /lithium_tbf

CMD gdb -batch -x ./gdb.cmds --args /lithium_tbf tfb-database 8080
