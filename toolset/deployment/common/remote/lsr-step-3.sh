#!/bin/bash  
#
# Bash script to be executed on the Linux server.
#
# Step 3: Verify setup.
#
echo "Host:" `hostname`
echo "Step 3: Verify setup."

export DEBIAN_FRONTEND=noninteractive
source ~/benchmark-configuration.sh

echo ""
echo "Running a smoke test..."
echo "servlet-raw (Java/MySQL), cpoll-pool(C++/PostgreSQL), nodejs-mongodb(JavaScript/MongoDB)"
cd "$BENCHMARK_HOME" || { echo "Error changing directory."; exit 1; }

echo "Verifying servlet-raw"
echo ""
toolset/run-tests.py -s "$BENCHMARK_SERVER_IP" -c "$BENCHMARK_CLIENT_IP" -i "$BENCHMARK_KEY_PATH" --max-threads 1 --name smoketest --test servlet-raw --type all -m verify

echo "Verifying cpoll_cppsp-postgres-raw"
echo ""
toolset/run-tests.py -s "$BENCHMARK_SERVER_IP" -c "$BENCHMARK_CLIENT_IP" -i "$BENCHMARK_KEY_PATH" --max-threads 1 --name smoketest --test cpoll_cppsp-postgres-raw --type all -m verify

echo "Verifying nodejs-mongodb"
echo ""
toolset/run-tests.py -s "$BENCHMARK_SERVER_IP" -c "$BENCHMARK_CLIENT_IP" -i "$BENCHMARK_KEY_PATH" --max-threads 1 --name smoketest --test nodejs-mongodb --type all -m verify
