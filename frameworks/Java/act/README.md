# ActFramework Benchmarking Test

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using [ActFramework](http://actframework.org) to build an application.

## How to run this app locally

You can easily run this sample application with mongodb and mysql, postgresql setup correctly.
For that, type:

```
mvn clean package
cd target/dist
unzip *.zip
./start
```

Or simply run `run.sh`
    

## Application Endpoints

### JSON serialization

* http://localhost:8080/json

### Single database query

* `mongo`: http://localhost:8080/mongo/db
* `mysql`: http://localhost:8080/mysql/db
* `pgsql`: http://localhost:8080/pgsql/db

### Multiple queries

* `mongo`: http://localhost:8080/mongo/queries?quries=20
* `mysql`: http://localhost:8080/mysql/queries?quries=20
* `pgsql`: http://localhost:8080/pgsql/queries?quries=20

### Fortunes

* `mongo`: http://localhost:8080/mongo/fortunes
* `mysql`: http://localhost:8080/mysql/fortunes
* `pgsql`: http://localhost:8080/pgsql/fortunes

### DB updates

* `mongo`: http://localhost:8080/mongo/updates?quries=20
* `mysql`: http://localhost:8080/mysql/updates?quries=20
* `pgsql`: http://localhost:8080/pgsql/updates?quries=20

### Plain text

* mongo: http://localhost:8080/plaintext

## Source code

### App entry and non-database request handlers

* [App entry](src/main/java/com/techempower/act/AppEntry.java)
* [json and plaintext handler](src/main/java/com/techempower/act/controller/HelloController.java)

### Database relevant source files

This application is organized to support access three database sources: `mongo`, `mysql`, `pgsql`. 
However most of the logic is the same when handling request sent to different data sources. We have
organized the project in a way to maximize the reuse of coding for the same logic. Each piece of source
unit are put into four groups: 

* `common`: encapsulate the common logic or data models
* `mongo` : the mongodb adaptor
* `mysql` : the mysql adaptor
* `pgsql` : the postgresql adaptor

#### Domain Models

##### Common

* [Fortune](src/main/java/com/techempower/act/domain/IFortune.java)
* [World](src/main/java/com/techempower/act/domain/IWorld.java)

##### Mongo

* [Fortune](src/main/java/com/techempower/act/mongo/domain/Fortune.java)
* [World](src/main/java/com/techempower/act/mongo/domain/World.java)

##### SQL Common 

The common base classes for mysql and pgsql classes

* [Fortune](src/main/java/com/techempower/act/sql/domain/Fortune.java)
* [World](src/main/java/com/techempower/act/sql/domain/World.java)

##### MySQL

* [Fortune](src/main/java/com/techempower/act/mysql/domain/Fortune.java)
* [World](src/main/java/com/techempower/act/mysql/domain/World.java)

##### Postgresql

* [Fortune](src/main/java/com/techempower/act/pgsql/domain/Fortune.java)
* [World](src/main/java/com/techempower/act/pgsql/domain/World.java)

#### Controllers

##### Common

* [Fortune controller](src/main/java/com/techempower/act/controller/FortuneControllerBase.java) - for `fortunes` endpoint
* [World controller](src/main/java/com/techempower/act/controller/WorldControllerBase.java) - for `db`, `queries` and `updates` endpoint

##### Mongo

* [Fortune controller](src/main/java/com/techempower/act/mongo/controller/FortuneController.java) - for `/mongo/fortunes` endpoint
* [World controller](src/main/java/com/techempower/act/mongo/controller/WorldController.java) - for `/mongo/db`, `/mongo/queries` and `/mongo/updates` endpoint

##### SQL Common

The common base classes for mysql and pgsql classes

* [Fortune controller](src/main/java/com/techempower/act/sql/controller/FortuneController.java) 
* [World controller](src/main/java/com/techempower/act/sql/controller/WorldController.java) 


##### MySQL

* [Fortune controller](src/main/java/com/techempower/act/mysql/controller/FortuneController.java) - for `/mysql/fortunes` endpoint
* [World controller](src/main/java/com/techempower/act/mysql/controller/WorldController.java) - for `/mysql/db`, `/mysql/queries` and `/mysql/updates` endpoint

##### Postgresql

* [Fortune controller](src/main/java/com/techempower/act/pgsql/controller/FortuneController.java) - for `/pgsql/fortunes` endpoint
* [World controller](src/main/java/com/techempower/act/pgsql/controller/WorldController.java) - for `/pgsql/db`, `/pgsql/queries` and `/pgsql/updates` endpoint



## Infrastructure Software Versions
The tests were run with:

* [ActFramework 0.4.0-SNAPSHOT](http://actframework.org/)
* [act-ebean plugin 0.4.0-SNAPSHOT](https://github.com/actframework/act-ebean)
* [act-morphia plugin 0.5.0-SNAPSHOT](https://github.com/actframework/act-morphia)
* [Undertow 1.4.8.Final](http://undertow.io/)


## Local Test Result

|            Test            | Throughput |  Latency  | Timeout |
| -------------------------- | ---------: | --------: | ------: |
| json                       |  828527.50 |    4.43ms |         |
| single query - mongo       |   44575.49 |   15.12ms |         |
| single query - mysql       |   25300.02 |   55.57ms |         |
| single query - pgsql       |   36930.28 |   48.51ms |         |
| 20 queries - mongo         |    2655.75 |  107.18ms |         |
| 20 queries - mysql         |    1225.00 |       n/a |     768 |
| 20 queries - pgsql         |    2010.60 |  386.54ms |      22 |
| fortunes - mongo           |   22417.17 |   48.37ms |         |
| fortunes - mysql           |   22326.85 |   47.14ms |         |
| fortunes - pgsql           |   24216.44 |   85.19ms |         |
| 20 updates - mongo         |    2250.82 |  562.06ms |         |
| 20 updates - mysql         |     408.33 |       n/a |     256 |
| 20 updates - pgsql         |     707.77 |       n/a |     444 |
| plaintext                  | 1361223.78 |    2.36ms |         |


### Local Test Logs

#### JSON Serialization

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /json 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.43ms    4.34ms 202.38ms   90.32%
    Req/Sec   209.33k    59.03k  389.44k    62.25%
  Latency Distribution
     50%    3.70ms
     75%    6.09ms
     90%    8.69ms
     99%   15.91ms
  8359840 requests in 10.09s, 1.34GB read
Requests/sec: 828527.50
Transfer/sec:    135.91MB
```

#### Plain text

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /plaintext 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.36ms    2.08ms  39.37ms   85.72%
    Req/Sec   344.48k    82.82k  691.36k    66.25%
  Latency Distribution
     50%    1.89ms
     75%    3.16ms
     90%    4.58ms
     99%   10.10ms
  13751216 requests in 10.10s, 1.95GB read
Requests/sec: 1361223.78
Transfer/sec:    197.32MB
```

#### Single Query - MongoDB

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/db 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    15.12ms   30.09ms 126.68ms    0.00%
    Req/Sec    11.23k   657.00    13.74k    67.75%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  447408 requests in 10.04s, 75.00MB read
Requests/sec:  44575.49
Transfer/sec:      7.47MB
```

#### Single query - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/db 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    55.57ms   50.98ms 326.24ms    5.78%
    Req/Sec     6.39k     1.07k   10.34k    78.09%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  253872 requests in 10.03s, 42.56MB read
Requests/sec:  25300.02
Transfer/sec:      4.24MB
```

#### Single query - Postgres

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/db 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    48.51ms   32.12ms 335.31ms   28.60%
    Req/Sec     9.33k     0.93k   12.61k    89.67%
  Latency Distribution
     50%  109.70ms
     75%    0.00us
     90%    0.00us
     99%    0.00us
  370144 requests in 10.02s, 62.05MB read
Requests/sec:  36930.28
Transfer/sec:      6.19MB
```

#### 20 queries - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/queries?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   107.18ms  361.87ms   1.52s     0.00%
    Req/Sec     2.07k     2.81k    8.96k    82.14%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  26720 requests in 10.06s, 19.94MB read
Requests/sec:   2655.75
Transfer/sec:      1.98MB
```

#### 20 queries - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/queries?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.00us    0.00us   0.00us    -nan%
    Req/Sec     1.76k     1.97k    5.82k    78.69%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  12288 requests in 10.03s, 9.17MB read
  Socket errors: connect 0, read 0, write 0, timeout 768
Requests/sec:   1225.00
Transfer/sec:      0.91MB
```

#### 20 queries - Postgres
 
```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/queries?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   386.54ms  624.21ms   2.00s     0.00%
    Req/Sec     1.10k     1.14k    4.36k    82.18%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  20208 requests in 10.05s, 15.08MB read
  Socket errors: connect 0, read 0, write 0, timeout 22
Requests/sec:   2010.60
Transfer/sec:      1.50MB
```

#### 20 updates - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/updates?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   562.06ms  567.30ms   1.98s     1.11%
    Req/Sec     1.57k     1.62k    7.43k    80.83%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  22624 requests in 10.05s, 16.89MB read
Requests/sec:   2250.82
Transfer/sec:      1.68MB
```

#### 20 updates - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/updates?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.00us    0.00us   0.00us    -nan%
    Req/Sec     1.06k     1.02k    3.39k    64.52%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  4096 requests in 10.03s, 3.06MB read
  Socket errors: connect 0, read 0, write 0, timeout 256
Requests/sec:    408.33
Transfer/sec:    312.06KB
```

#### 20 updates - Postgresql

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/updates?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.00us    0.00us   0.00us    -nan%
    Req/Sec     1.21k     1.28k    4.80k    84.78%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  7104 requests in 10.04s, 5.30MB read
  Socket errors: connect 0, read 0, write 0, timeout 444
Requests/sec:    707.77
Transfer/sec:    540.90KB
```

#### Fortunes - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/fortunes 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    48.37ms   62.79ms 283.94ms    0.00%
    Req/Sec     5.66k     1.15k   10.08k    77.02%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  224908 requests in 10.03s, 313.15MB read
Requests/sec:  22417.17
Transfer/sec:     31.21MB
```

#### Fortunes - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/fortunes 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    47.14ms   62.73ms 291.87ms    0.00%
    Req/Sec     5.64k     1.34k    9.76k    73.43%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  224776 requests in 10.07s, 312.97MB read
Requests/sec:  22326.85
Transfer/sec:     31.09MB
```

#### Fortunes - Postgresql

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/fortunes 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    85.19ms   40.72ms 378.41ms   50.28%
    Req/Sec     6.26k     1.75k   13.66k    86.67%
  Latency Distribution
     50%  123.14ms
     75%  169.66ms
     90%    0.00us
     99%    0.00us
  242972 requests in 10.03s, 338.31MB read
Requests/sec:  24216.44
Transfer/sec:     33.72MB
```
