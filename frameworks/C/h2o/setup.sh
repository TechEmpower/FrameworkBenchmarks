#!/bin/bash

fw_depends postgresql h2o mustache-c yajl

H2O_APP_HOME="${IROOT}/h2o_app"
BUILD_DIR="${H2O_APP_HOME}_build"
H2O_APP_PROFILE_PORT=54321
H2O_APP_PROFILE_URL="http://127.0.0.1:$H2O_APP_PROFILE_PORT"
PHYSICAL_ENVIRONMENT_THREADS=8

# A hacky way to detect whether we are running in the physical hardware or the cloud environment.
if [[ "$CPU_COUNT" -gt 16 ]]; then
	CLOUD_ENVIRONMENT=false
	# In the physical hardware environment the number of threads used by the application is not
	# the same as the number of logical CPU cores that the database server has, so we need to
	# adjust the maximum number of database connections per thread accordingly.
	DB_CONN=15
else
	CLOUD_ENVIRONMENT=true
	DB_CONN=16
fi

build_h2o_app()
{
	cmake -DCMAKE_INSTALL_PREFIX="$H2O_APP_HOME" -DCMAKE_BUILD_TYPE=Release \
	      -DCMAKE_PREFIX_PATH="${H2O_HOME};${MUSTACHE_C_HOME};${YAJL_HOME}" \
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
		-d "host=TFB-database dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass" &
}

generate_profile_data()
{
	run_h2o_app 0 . "${TROOT}" -p$H2O_APP_PROFILE_PORT -t1
	local -r H2O_APP_PROFILE_PID=$!
	while ! curl ${H2O_APP_PROFILE_URL} > /dev/null 2>&1; do sleep 1; done
	run_curl json
	run_curl db
	run_curl queries?queries=20
	run_curl fortunes
	run_curl updates?queries=20
	run_curl plaintext
	run_curl cached-worlds?queries=20
	kill -s SIGTERM $H2O_APP_PROFILE_PID
	wait $H2O_APP_PROFILE_PID
}

install -d "$BUILD_DIR"
pushd "$BUILD_DIR"
build_h2o_app "-fprofile-generate"
generate_profile_data
make clean
rm -f CMakeCache.txt
build_h2o_app "-fprofile-use"
make -j "$CPU_COUNT" install
popd
rm -rf "$BUILD_DIR"
echo "Maximum database connections per thread: $DB_CONN"

if "$CLOUD_ENVIRONMENT"; then
	run_h2o_app 0 "${H2O_APP_HOME}/bin" "${H2O_APP_HOME}/share/h2o_app"
else
	for ((i = 0; i < PHYSICAL_ENVIRONMENT_THREADS; i++)); do
		run_h2o_app "$i" "${H2O_APP_HOME}/bin" "${H2O_APP_HOME}/share/h2o_app" -t1
	done
fi
