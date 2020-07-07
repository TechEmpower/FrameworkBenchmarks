FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev g++-9

COPY ./ ./

ENV COMMIT=b7a9000a92b53d45658cd3273f8ab898325055eb

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -DNDEBUG -DTFB_MYSQL -O3 -march=native -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -o /lithium_tbf

CMD /lithium_tbf tfb-database 8080
