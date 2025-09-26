#! /bin/sh

DB_FLAG=$1
COMMIT=10b72ad8ce16528ff642f802e266a975964d06f9
COMMIT="$2"
MONOTHREAD=$2	
if [ "$COMMIT" = "" ]; then
  COMMIT="10b72ad8ce16528ff642f802e266a975964d06f9"
fi

if [ $DB_FLAG = "TFB_MYSQL" ]; then
  echo "ERROR: Only Postgres has pipelining support for now."
  exit 1
  CXX_FLAGS="-I /usr/include/mariadb  -lmariadbclient "
  wget -nv https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
elif [ $DB_FLAG = "TFB_PGSQL" ]; then
  CXX_FLAGS="-lpthread  -L/usr/lib -lpq -I/postgres-bab150045bd9766869f471ede88734ea0989261c/src/include"
  wget -nv https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_pgsql.hh
fi


wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

clang++ -fprofile-instr-generate=./profile.prof -flto -DLITHIUM_SERVER_NAME=l -DPROFILE_MODE -DN_SQL_CONNECTIONS=1 -DNDEBUG -D$DB_FLAG -O3 -march=native -std=c++17 ./lithium_batch.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
/lithium_tbf tfb-database 8081
llvm-profdata-10 merge -output=./profile.pgo  ./profile.prof
clang++ -fprofile-instr-use=./profile.pgo -flto -DLITHIUM_SERVER_NAME=l -DNDEBUG -D$DB_FLAG -DN_SQL_CONNECTIONS=1 -O3 -march=native -std=c++17 ./lithium_batch.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf

/lithium_tbf tfb-database 8080
