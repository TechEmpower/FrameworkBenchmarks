FROM buildpack-deps:focal

RUN apt update -yqq
RUN apt install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev g++-9 gdb

COPY ./ ./

ENV COMMIT=0eca23f041ac103993d2c41248dfae41f4369e98

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -DNDEBUG -g -DTFB_MYSQL -O3 -march=native -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -o /lithium_tbf

#CMD gdb -batch -x ./gdb.cmds --args /lithium_tbf tfb-database 8080
CMD /lithium_tbf tfb-database 8080
