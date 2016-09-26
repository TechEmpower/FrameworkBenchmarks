#!/bin/bash

fw_depends h2o mustache-c yajl

H2O_APP_HOME="${IROOT}/h2o_app"
BUILD_DIR="${H2O_APP_HOME}_build"
H2O_APP_PROFILE_PORT="54321"
H2O_APP_PROFILE_URL="http://127.0.0.1:$H2O_APP_PROFILE_PORT"

build_h2o_app()
{
	cmake -DCMAKE_INSTALL_PREFIX="$H2O_APP_HOME" -DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_LIBRARY_PATH="${H2O_HOME}/lib;${MUSTACHE_C_HOME}/lib;${YAJL_HOME}/lib" \
		-DCMAKE_INCLUDE_PATH="${H2O_HOME}/include;${MUSTACHE_C_HOME}/include;${YAJL_HOME}/include" \
		-DCMAKE_C_FLAGS="-march=native $1" "$TROOT"
	make -j "$(nproc)"
}

run_curl()
{
	for ((i = 0; i < 10; i++)); do
		curl "${H2O_APP_PROFILE_URL}/$1" > /dev/null 2>&1
	done
}

run_h2o_app()
{
	"$1/h2o_app" -a2 -f "$2/template/fortunes.mustache" -m8 "$3" "$4" \
		-d "host=$DBHOST dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass" &
}

generate_profile_data()
{
	run_h2o_app . "${TROOT}" -p$H2O_APP_PROFILE_PORT -t1
	local -r H2O_APP_PROFILE_PID=$!
	while ! curl ${H2O_APP_PROFILE_URL} > /dev/null 2>&1; do sleep 1; done
	run_curl json
	run_curl db
	run_curl queries?queries=20
	run_curl fortunes
	run_curl updates?queries=20
	run_curl plaintext
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
make -j "$(nproc)" install
popd
rm -rf "$BUILD_DIR"
run_h2o_app "${H2O_APP_HOME}/bin" "${H2O_APP_HOME}/share/h2o_app"
