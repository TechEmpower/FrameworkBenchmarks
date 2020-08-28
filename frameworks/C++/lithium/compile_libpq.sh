#! /bin/sh

# Compile libpq
wget -nv https://github.com/postgres/postgres/archive/bab150045bd9766869f471ede88734ea0989261c.zip

unzip -q bab150045bd9766869f471ede88734ea0989261c.zip
cd postgres-bab150045bd9766869f471ede88734ea0989261c
if [ "$1" = "batchmode" ]; then
  wget -nv https://www.postgresql.org/message-id/attachment/112272/v18-0001-libpq-batch-support.patch
  git apply ./v18-0001-libpq-batch-support.patch
fi

./configure --prefix=/usr CFLAGS='-O3 -march=native -flto'
cd src/interfaces/libpq
make all install -j4
cd /
