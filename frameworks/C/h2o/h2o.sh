#!/bin/bash

set -e

CPU_COUNT=$(nproc)
H2O_APP_PROFILE_PORT=54321
H2O_APP_PROFILE_URL="http://127.0.0.1:$H2O_APP_PROFILE_PORT"
SCRIPT_PATH=$(realpath "$0")
H2O_APP_SRC_ROOT=$(dirname "$SCRIPT_PATH")
H2O_APP_BUILD_DIR="${H2O_APP_SRC_ROOT}/build"
NUM_WORKERS="$CPU_COUNT"

if [[ -z "$DBHOST" ]]; then
	DBHOST=tfb-database
fi

if [[ -z "$H2O_APP_PREFIX" ]]; then
	H2O_APP_PREFIX=/opt/h2o_app
fi

if [[ -z "$H2O_PREFIX" ]]; then
	H2O_PREFIX=/usr
fi

if [[ -z "$MUSTACHE_C_PREFIX" ]]; then
	MUSTACHE_C_PREFIX=/opt/mustache-c
fi

# A hacky way to detect whether we are running in the physical hardware or the cloud environment.
if [[ "$CPU_COUNT" -gt 16 ]]; then
	echo "Running h2o_app in the physical hardware environment."
	USE_PROCESSES=false
	DB_CONN=3
else
	echo "Running h2o_app in the cloud environment."
	USE_PROCESSES=false
	DB_CONN=5
fi

build_h2o_app()
{
	cmake -DCMAKE_INSTALL_PREFIX="$H2O_APP_PREFIX" -DCMAKE_BUILD_TYPE=Release \
	      -DCMAKE_PREFIX_PATH="${H2O_PREFIX};${MUSTACHE_C_PREFIX}" \
	      -DCMAKE_C_FLAGS="-march=native $1" "$H2O_APP_SRC_ROOT"
	make -j "$CPU_COUNT"
}

run_curl()
{
	for ((i = 0; i < 10; i++)); do
		curl "${H2O_APP_PROFILE_URL}/$1" > /dev/null 2>&1
	done
}

run_h2o_app()
{
	taskset -c "$1" "$2/h2o_app" -a20 -f "$3/template" -m "$DB_CONN" "$4" "$5" \
	        -d "host=$DBHOST dbname=hello_world user=benchmarkdbuser sslmode=disable \
	            password=benchmarkdbpass" &
}

generate_profile_data()
{
	run_h2o_app 0 . "$H2O_APP_SRC_ROOT" "-p$H2O_APP_PROFILE_PORT" -t1
	local -r H2O_APP_PROFILE_PID=$!
	while ! curl "$H2O_APP_PROFILE_URL" > /dev/null 2>&1; do sleep 1; done
	run_curl json
	run_curl db
	run_curl queries?queries=20
	run_curl fortunes
	run_curl updates?queries=20
	run_curl plaintext
	run_curl cached-worlds?queries=20
	kill -s SIGTERM "$H2O_APP_PROFILE_PID"
	wait "$H2O_APP_PROFILE_PID"
}

install -d "$H2O_APP_BUILD_DIR"
pushd "$H2O_APP_BUILD_DIR"
build_h2o_app "-fprofile-generate"
generate_profile_data
make clean
rm -f CMakeCache.txt
build_h2o_app "-fprofile-use"
make -j "$CPU_COUNT" install
popd
rm -rf "$H2O_APP_BUILD_DIR"
echo "Maximum database connections per thread: $DB_CONN"
export LD_LIBRARY_PATH="${MUSTACHE_C_PREFIX}/lib:$LD_LIBRARY_PATH"

if "$USE_PROCESSES"; then
	echo "h2o_app processes: $NUM_WORKERS"

	for ((i = 0; i < NUM_WORKERS; i++)); do
		run_h2o_app "$i" "${H2O_APP_PREFIX}/bin" "${H2O_APP_PREFIX}/share/h2o_app" -t1
	done
else
	echo "Running h2o_app multithreaded."
	run_h2o_app 0 "${H2O_APP_PREFIX}/bin" "${H2O_APP_PREFIX}/share/h2o_app"
fi

wait
