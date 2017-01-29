# ActFramework Benchmarking Test

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using [ActFramework](http://actframework.org) to build an application.

## How to run this app locally

You can easily run this sample application with mongodb or an embedded H2 database.
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

* [ActFramework 0.3.1-SNAPSHOT](http://actframework.org/)
* [act-ebean plugin 0.3.1-SNAPSHOT](https://github.com/actframework/act-ebean)
* [act-morphia plugin 0.4.1-SNAPSHOT](https://github.com/actframework/act-morphia)
* [Undertow 1.4.8.Final](http://undertow.io/)


## Local Test Result

|            Test            | Throughput |  Latency  | Timeout |
| -------------------------- | ---------- | --------- | ------- |
| json                       |  340625.81 |    7.42ms |         |
| single query - mongo       |   42852.18 |   25.83ms |         |
| single query - mysql       |   23783.32 |   46.66ms |         |
| single query - pgsql       |   33442.32 |   39.91ms |         |
| 20 queries - mongo         |    2444.14 |  195.34ms |         |
| 20 queries - mysql         |    1224.91 |       n/a |     768 |
| 20 queries - pgsql         |    1792.12 |  453.02ms |     873 |
| 20 updates - mongo         |    2031.09 |  167.19ms |         |
| 20 updates - mysql         |     408.35 |       n/a |     256 |
| 20 updates - pgsql         |     815.00 |       n/a |     512 |
| fortunes - mongo           |   22062.14 |   87.88ms |         |
| fortunes - mysql           |   20337.16 |   30.38ms |         |
| fortunes - pgsql           |   22272.51 |   51.23ms |         |
| plaintext                  |  346660.26 |    7.79ms |         |


### Local Test Logs

#### JSON Serialization

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /json 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     7.42ms    5.02ms 219.77ms   71.96%
    Req/Sec    85.76k     9.76k  119.11k    68.25%
  Latency Distribution
     50%    6.71ms
     75%   10.32ms
     90%   13.38ms
     99%   22.45ms
  3412544 requests in 10.02s, 559.77MB read
Requests/sec: 340625.81
Transfer/sec:     55.87MB
```

#### Plain text

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /plaintext 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     7.97ms    9.13ms 224.80ms   94.67%
    Req/Sec    87.31k    11.06k  120.00k    74.50%
  Latency Distribution
     50%    6.64ms
     75%   10.46ms
     90%   14.16ms
     99%   27.18ms
  3475120 requests in 10.02s, 553.46MB read
Requests/sec: 346660.26
Transfer/sec:     55.21MB
```

#### Single Query - MongoDB

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/db 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    25.83ms   32.47ms 134.19ms    0.00%
    Req/Sec    10.81k   823.82    15.03k    83.00%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  430768 requests in 10.05s, 72.21MB read
Requests/sec:  42852.18
Transfer/sec:      7.18MB
```

#### Single query - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/db 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    46.66ms   58.28ms 221.56ms    0.00%
    Req/Sec     6.01k     1.45k   10.18k    76.26%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  238656 requests in 10.03s, 40.01MB read
Requests/sec:  23783.32
Transfer/sec:      3.99MB
```

#### Single query - Postgres

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/db 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    39.91ms   42.89ms 247.65ms    1.56%
    Req/Sec     8.44k   771.49    10.56k    82.71%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  335936 requests in 10.05s, 56.32MB read
Requests/sec:  33442.32
Transfer/sec:      5.61MB
```

#### 20 queries - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/queries?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   195.34ms  466.91ms   1.62s     0.00%
    Req/Sec     2.71k     3.28k    9.92k    78.72%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  24576 requests in 10.06s, 18.34MB read
Requests/sec:   2444.14
Transfer/sec:      1.82MB
```

#### 20 queries - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/queries?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.00us    0.00us   0.00us    -nan%
    Req/Sec     2.51k     2.64k    8.16k    77.27%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  12288 requests in 10.03s, 9.17MB read
  Socket errors: connect 0, read 0, write 0, timeout 768
Requests/sec:   1224.91
Transfer/sec:      0.91MB
```

#### 20 queries - Postgres
 
```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/queries?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   453.02ms  611.50ms   2.00s     0.00%
    Req/Sec     1.01k     0.85k    4.32k    74.71%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  18000 requests in 10.04s, 13.44MB read
  Socket errors: connect 0, read 0, write 0, timeout 873
Requests/sec:   1792.12
Transfer/sec:      1.34MB
```

#### 20 updates - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/updates?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   167.19ms  495.91ms   1.88s     0.00%
    Req/Sec     2.89k     3.53k    9.44k    75.00%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  20480 requests in 10.08s, 15.28MB read
Requests/sec:   2031.09
Transfer/sec:      1.52MB
```

#### 20 updates - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/updates?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.00us    0.00us   0.00us    -nan%
    Req/Sec     1.47k     1.67k    5.33k    79.17%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  4096 requests in 10.03s, 3.06MB read
  Socket errors: connect 0, read 0, write 0, timeout 256
Requests/sec:    408.35
Transfer/sec:    312.06KB
```

#### 20 updates - Postgresql

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/updates?queries=20 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.00us    0.00us   0.00us    -nan%
    Req/Sec     2.16k     2.02k    6.88k    57.58%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  8192 requests in 10.05s, 6.11MB read
  Socket errors: connect 0, read 0, write 0, timeout 512
Requests/sec:    815.00
Transfer/sec:    622.85KB
```

#### Fortunes - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/fortunes 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    87.88ms   45.47ms 397.54ms   39.55%
    Req/Sec     5.58k     1.10k    9.57k    73.75%
  Latency Distribution
     50%  152.88ms
     75%    0.00us
     90%    0.00us
     99%    0.00us
  222176 requests in 10.07s, 306.17MB read
Requests/sec:  22062.14
Transfer/sec:     30.40MB
```

#### Fortunes - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/fortunes 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    30.38ms   64.32ms 274.78ms    0.00%
    Req/Sec     5.14k     1.98k    9.74k    73.99%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  204644 requests in 10.06s, 282.01MB read
Requests/sec:  20337.16
Transfer/sec:     28.03MB
```

#### Fortunes - Postgresql

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/webframework-benchmark$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/fortunes 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    51.23ms   63.86ms 324.07ms    0.00%
    Req/Sec     5.62k     1.00k   10.14k    79.85%
  Latency Distribution
     50%    0.00us
     75%    0.00us
     90%    0.00us
     99%    0.00us
  223780 requests in 10.05s, 308.38MB read
Requests/sec:  22272.51
Transfer/sec:     30.69MB
```
