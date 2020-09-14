#! /bin/sh

DB_FLAG=$1
COMMIT=2ea53ab949627baeaaad77e7662da74f9a9d0a31

if [ $DB_FLAG = "TFB_MYSQL" ]; then
  CXX_FLAGS="-I /usr/include/mariadb  -lmariadbclient "
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
elif [ $DB_FLAG = "TFB_PGSQL" ]; then
  CXX_FLAGS="-I/usr/include/postgresql -I /usr/include/postgresql/12/server -lpthread -lpq"
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_pgsql.hh
fi


wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

clang++ -fprofile-instr-generate=./profile.prof -flto -DPROFILE_MODE -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
/lithium_tbf tfb-database 8081
llvm-profdata-10 merge -output=./profile.pgo  ./profile.prof
clang++ -fprofile-instr-use=./profile.pgo -flto -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf

/lithium_tbf tfb-database 8080
