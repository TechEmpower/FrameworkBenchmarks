# t-io Benchmarking Test

This is the tio-server portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Controller

These implementations use the tio-server's controller.

### Plaintext Test

* [Plaintext test source](src/main/java/com/litongjava/tio/http/server/controller/IndexController.java)

### JSON Serialization Test

* [JSON test source](src/main/java/com/litongjava/tio/http/server/controller/IndexController.java)

### Database Query Test

* [Database Query test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Database Queries Test

* [Database Queries test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Database Update Test

* [Database Update test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Template rendering Test

* [Template rendering test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Cache Query Test
* [Cache query test source](src/main/java/com/litongjava/tio/http/server/controller/CacheController.java))


## Versions
3.7.3.v20231218-RELEASE (https://gitee.com/litongjava/t-io)

## Test URLs

All implementations use the same URLs.

### Plaintext Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Database Query Test

    http://localhost:8080/db

### Database Queries Test

    http://localhost:8080/queries?queries=5

### Cache Query Test

    http://localhost:8080/cacheQuery?queries=10000

### Template rendering Test

    http://localhost:8080/fortunes
    
### Database Update Test

    http://localhost:8080/updates?queries=5

 ## Hot to run
 ### install mysql 8
 - 1.please instal mysql 8.0.32,example cmd
 ```
 docker run --restart=always -d --name mysql_8 --hostname mysql \
-p 3306:3306 \
-e 'MYSQL_ROOT_PASSWORD=robot_123456#' -e 'MYSQL_ROOT_HOST=%' -e 'MYSQL_DATABASE=hello_world' \
mysql/mysql-server:8.0.32 \
--character-set-server=utf8mb4 --collation-server=utf8mb4_unicode_ci --lower_case_table_names=1
 ```
 - 2.create database schema hello_world
 - 3.create tablle,[example](sql/hello_world.sql)
 - 4.import data
 
 ### docker 
 ```
 docker build -t tio-server-benchmark -f tio-server.dockerfile .
```
The run is to specify the mysql database
```
docker run --rm -p 8080:8080 \
-e JDBC_URL="jdbc:mysql://192.168.3.9/hello_world" \
-e JDBC_USER="root" \
-e JDBC_PSWD="robot_123456#" \
tio-server-benchmark
```

### windows

-windows
```
D:\java\jdk1.8.0_121\bin\java -jar target\tio-server-benchmark-1.0.jar --JDBC_URL=jdbc:mysql://192.168.3.9/hello_world?useSSL=false --JDBC_USER=root --JDBC_PSWD=robot_123456#
```
or 
```
set JDBC_URL=jdbc:mysql://192.168.3.9/hello_world
set jdbc.user=root
set JDBC_PSWD=robot_123456#
D:\java\jdk1.8.0_121\bin\java -jar target\tio-server-benchmark-1.0.jar
```



