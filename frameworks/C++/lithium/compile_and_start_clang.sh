#! /bin/sh

DB_FLAG=$1
COMMIT=58334f62653a330863cfff633b50f3cfc567e527

if [ $DB_FLAG = "TFB_MYSQL" ]; then
  CXX_FLAGS="-I /usr/include/mariadb  -lmariadbclient "
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
elif [ $DB_FLAG = "TFB_PGSQL" ]; then
  CXX_FLAGS="-lpthread  -L/usr/lib -lpq -I/postgres-bab150045bd9766869f471ede88734ea0989261c/src/include"
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_pgsql.hh
fi


wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

clang++ -fprofile-instr-generate=./profile.prof -flto -DPROFILE_MODE -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
/lithium_tbf tfb-database 8081
llvm-profdata-10 merge -output=./profile.pgo  ./profile.prof
clang++ -fprofile-instr-use=./profile.pgo -flto -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf

/lithium_tbf tfb-database 8080
