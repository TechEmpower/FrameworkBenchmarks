FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev clang-10

COPY ./ ./

ENV COMMIT=ba3849b86e004798faf91d64a1a9550e379ac53a

RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
RUN wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

RUN clang++ -fprofile-instr-generate=/tmp/profile.prof -DPROFILE_MODE -DNDEBUG -DTFB_MYSQL -O3 -march=native -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -lssl -lcrypto -o /lithium_tbf
CMD /lithium_tbf tfb-database 8080
RUN llvm-profdata-10 merge -output=/tmp/profile.pgo  /tmp/profile.prof
RUN clang++ -fprofile-instr-use=/tmp/profile.pgo -DNDEBUG -DTFB_MYSQL -O3 -march=native -std=c++17 ./lithium.cc -I /usr/include/mariadb -lpthread -lmariadbclient -lboost_context -lssl -lcrypto -o /lithium_tbf

CMD /lithium_tbf tfb-database 8080
