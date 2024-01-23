#! /bin/sh

DB_FLAG=$1
COMMIT="$2"

if [ "$COMMIT" = "" ]; then
  COMMIT="a046b3345098157849d9e2ab49a475aaabf4a90f"
fi

if [ $DB_FLAG = "TFB_MYSQL" ]; then
  CXX_FLAGS="-I /usr/include/mariadb  -lmariadbclient "
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
elif [ $DB_FLAG = "TFB_PGSQL" ]; then
  CXX_FLAGS="-lpthread  -L/usr/lib -lpq -I/postgres-bab150045bd9766869f471ede88734ea0989261c/src/include"
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_pgsql.hh
fi

COMPILER="clang++"

wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_server.hh

if [ $COMPILER = "clang++" ]; then
  clang++ -fprofile-instr-generate=./profile.prof -flto -DLITHIUM_SERVER_NAME=l -DPROFILE_MODE -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
  /lithium_tbf tfb-database 8081
  llvm-profdata-10 merge -output=./profile.pgo  ./profile.prof
  clang++ -fprofile-instr-use=./profile.pgo -flto -DLITHIUM_SERVER_NAME=l -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
else
  g++ -flto -DLITHIUM_SERVER_NAME=l -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
fi

/lithium_tbf tfb-database 8080
