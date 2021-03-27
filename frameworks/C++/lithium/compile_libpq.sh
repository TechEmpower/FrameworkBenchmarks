#! /bin/sh

# Compile libpq


if [ "$1" = "batchmode_patch" ]; then
  commit=b787d4ce6d910080065025bcd5f968544997271f
  wget -nv https://github.com/postgres/postgres/archive/$commit.zip
  unzip -q $commit.zip
  cd postgres-$commit
  wget -nv https://www.postgresql.org/message-id/attachment/115223/v22-0001-libpq-batch.patch
  git apply ./v22-0001-libpq-batch.patch
else
  commit=7f7f25f15edb6eacec58179ef5285e874aa4435b
  wget -nv https://github.com/postgres/postgres/archive/$commit.zip
  unzip -q $commit.zip
  cd postgres-$commit
fi

./configure --prefix=/usr CFLAGS='-O3 -march=native -flto'
cd src/interfaces/libpq
make all install -j4
cp ../../../src/include/postgres_ext.h ../../../src/include/pg_config_ext.h /usr/include
cd /
