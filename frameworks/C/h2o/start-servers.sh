#!/bin/bash

set -e

CPU_COUNT="$(nproc)"
H2O_APP_HOME=/h2o_app
H2O_APP_BUILD_DIR="${H2O_APP_HOME}_build"
H2O_APP_PROFILE_PORT=54321
H2O_APP_PROFILE_URL="http://127.0.0.1:$H2O_APP_PROFILE_PORT"
NUM_WORKERS="$CPU_COUNT"
TROOT=/

# A hacky way to detect whether we are running in the physical hardware or the cloud environment.
if [[ "$CPU_COUNT" -gt 16 ]]; then
	echo "Running h2o_app in the physical hardware environment."
	USE_PROCESSES=false
	DB_CONN=3
else
	echo "Running h2o_app in the cloud environment."
	USE_PROCESSES=false
	DB_CONN=8
fi

build_h2o_app()
{
	cmake -DCMAKE_INSTALL_PREFIX="$H2O_APP_HOME" -DCMAKE_BUILD_TYPE=Release \
	      -DCMAKE_PREFIX_PATH="${H2O_HOME};${MUSTACHE_C_HOME}" \
	      -DCMAKE_C_FLAGS="-march=native $1" "$TROOT"
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
	taskset -c "$1" "$2/h2o_app" -a20 -f "$3/template/fortunes.mustache" -m "$DB_CONN" "$4" "$5" \
	        -d "host=tfb-database dbname=hello_world user=benchmarkdbuser \
	            password=benchmarkdbpass" &
}

generate_profile_data()
{
	run_h2o_app 0 . "$TROOT" "-p$H2O_APP_PROFILE_PORT" -t1
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

if "$USE_PROCESSES"; then
	echo "h2o_app processes: $NUM_WORKERS"

	for ((i = 0; i < NUM_WORKERS; i++)); do
		run_h2o_app "$i" "${H2O_APP_HOME}/bin" "${H2O_APP_HOME}/share/h2o_app" -t1
	done
else
	echo "Running h2o_app multithreaded."
	run_h2o_app 0 "${H2O_APP_HOME}/bin" "${H2O_APP_HOME}/share/h2o_app"
fi

wait
