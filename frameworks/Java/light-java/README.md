# Light-Java Benchmarking Test

This is the light-java portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
* [JSON test source](src/main/java/hello/HelloWebServer.java)

## Versions
Light-Java 1.2.3 (https://github.com/networknt/light-java)

## Test URLs

### JSON Encoding Test

    http://localhost:8080
### Local build database

start database at light-java folder

docker run -it -p 5432:5432 -v $PWD/src/main/resources/script/postgres:/docker-entrypoint-initdb.d --env POSTGRES_PASSWORD=benchmarkdbpass --env POSTGRES_DB=hello_world postgres:9.6.0
docker run -it -p 3306:3306 -v $PWD/src/main/resources/script/mysql:/docker-entrypoint-initdb.d --env MYSQL_ROOT_PASSWORD=benchmarkdbpass mysql:5.7.16
