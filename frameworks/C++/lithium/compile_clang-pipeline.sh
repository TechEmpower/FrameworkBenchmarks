#! /bin/sh

DB_FLAG=$1
COMMIT=2d409878031f9c6ee7e7ef25535b8197fdd7d90c
MONOTHREAD=$2

# Remove conflicting libpq.
rm /usr/lib/libpq.*

# Compile libpq with pipelining support. 
wget -nv https://www.postgresql.org/message-id/attachment/112272/v18-0001-libpq-batch-support.patch
wget -nv https://github.com/postgres/postgres/archive/bab150045bd9766869f471ede88734ea0989261c.zip
unzip -q bab150045bd9766869f471ede88734ea0989261c.zip
cd postgres-bab150045bd9766869f471ede88734ea0989261c
git apply ../v18-0001-libpq-batch-support.patch
./configure --prefix=/usr CFLAGS='-O3 -march=native -flto'
cd src/interfaces/libpq
make all install -j4
cd /

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

echo "Compile server"
clang++ -flto -DNDEBUG -D$DB_FLAG -DMONOTHREAD=$MONOTHREAD -DN_SQL_CONNECTIONS=1 -O3 -march=native -std=c++17 ./lithium_pipeline.cc $CXX_FLAGS -lpthread -lboost_context -lssl -lcrypto -o /lithium_tbf
