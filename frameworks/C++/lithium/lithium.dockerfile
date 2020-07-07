FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev g++-9

COPY ./ ./

ENV COMMIT=0f9f356c2040a455a6adab53b4f642ccdd815a38

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -DNDEBUG -DTFB_MYSQL -O3 -march=native -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -lssl -lcrypto -o /lithium_tbf

CMD /lithium_tbf tfb-database 8080
