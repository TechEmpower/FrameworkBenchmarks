FROM buildpack-deps:focal

RUN apt update -yqq
RUN apt install -yqq g++-9 libboost-dev libmariadb-dev wget
RUN apt install -yqq libboost-context-dev

COPY ./ ./

ENV COMMIT=5f409d2f995f8e8b1998f4e1a914a18c09109e4b

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN g++ -DTFB_MYSQL -O3 -DNDEBUG -march=native -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -o /lithium_tbf

CMD /lithium_tbf tfb-database 8080 $(nproc)
