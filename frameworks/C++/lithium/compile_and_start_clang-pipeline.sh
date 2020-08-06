#! /bin/sh

DB_FLAG=$1
COMMIT=ae71a002630d17a7d79c5216ce477f7cd0bc1e37
MONOTHREAD=$2

# Remove conflicting libpq.
rm /usr/lib/libpq.*

# Compile libpq with pipelining support.
wget https://github.com/2ndQuadrant/postgres/archive/dev/libpq-async-batch.zip 
unzip -qq libpq-async-batch.zip
cd postgres-dev-libpq-async-batch
./configure --prefix=/usr CFLAGS='-O3 -march=native -flto'
cd src/interfaces/libpq
make all install -j4
cd /

if [ $DB_FLAG = "TFB_MYSQL" ]; then
  CXX_FLAGS="-I /usr/include/mariadb  -lmariadbclient "
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_mysql.hh
elif [ $DB_FLAG = "TFB_PGSQL" ]; then
  CXX_FLAGS="-lpthread  -L/usr/lib -lpq -I/postgres-dev-libpq-async-batch/src/include"
  wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_pgsql.hh
fi


wget https://raw.githubusercontent.com/matt-42/lithium/$COMMIT/single_headers/lithium_http_backend.hh

#echo "Compile profiling"
#clang++ -fprofile-instr-generate=./profile.prof -flto -DPROFILE_MODE -DNDEBUG -D$DB_FLAG -DN_SQL_CONNECTIONS=1 -DMONOTHREAD=$MONOTHREAD -O3 -march=native -std=c++17 ./lithium_pipeline.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
#echo "Run profiling"
# LD_LIBRARY_PATH=/usr/lib /lithium_tbf tfb-database 8081
# llvm-profdata-10 merge -output=./profile.pgo  ./profile.prof

# echo "Compile optimized binary"
# clang++ -fprofile-instr-use=./profile.pgo -flto -DNDEBUG -D$DB_FLAG -DMONOTHREAD=$MONOTHREAD -DN_SQL_CONNECTIONS=1 -O3 -march=native -std=c++17 ./lithium_pipeline.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf

echo "Compile server"
clang++ -flto -DNDEBUG -D$DB_FLAG -DMONOTHREAD=$MONOTHREAD -DN_SQL_CONNECTIONS=1 -O3 -march=native -std=c++17 ./lithium_pipeline.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf

echo "Start server"
LD_LIBRARY_PATH=/usr/lib /lithium_tbf tfb-database 8080
