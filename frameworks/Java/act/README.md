# ActFramework Benchmarking Test

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using [ActFramework R1.1.2](http://actframework.org) to build an application.

## How to run this app locally

You can easily run this sample application with mongodb and mysql, postgresql setup correctly.
For that, type:

```
mvn clean package
cd target/dist
unzip *.zip
./start
```

Or simply run `./run.sh`
    

## Application Endpoints

### JSON serialization

* http://localhost:8080/json

### Single database query

* `mongo`: http://localhost:8080/mongo/db
* `mysql`: http://localhost:8080/mysql/db
* `pgsql`: http://localhost:8080/pgsql/db
* `beetlsql-pgsql`: http://localhost:8080/beetsql/db
* `jdbc-pgsql`: http://localhost:8080/beetsql/db

### Multiple queries

* `mongo`: http://localhost:8080/mongo/queries?quries=20
* `mysql`: http://localhost:8080/mysql/queries?quries=20
* `pgsql`: http://localhost:8080/pgsql/queries?quries=20
* `beetlsql-pgsql`: http://localhost:8080/beetsql/queries?quries=20
* `jdbc-pgsql`: http://localhost:8080/beetsql/queries?quries=20

### Fortunes

* `mongo`: http://localhost:8080/mongo/fortunes
* `mysql`: http://localhost:8080/mysql/fortunes
* `pgsql`: http://localhost:8080/pgsql/fortunes
* `beetlsql-pgsql`: http://localhost:8080/beetsql/db
* `jdbc-pgsql`: http://localhost:8080/beetsql/db

### DB updates

* `mongo`: http://localhost:8080/mongo/updates?quries=20
* `mysql`: http://localhost:8080/mysql/updates?quries=20
* `pgsql`: http://localhost:8080/pgsql/updates?quries=20
* `beetlsql-pgsql`: http://localhost:8080/beetsql/updates?quries=20
* `jdbc-pgsql`: http://localhost:8080/beetsql/updates?quries=20

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

* [ActFramework 1.1.2](http://actframework.org/)
* [act-ebean2 plugin 1.04](https://github.com/actframework/act-ebean2)
* [act-morphia plugin 1.1.0](https://github.com/actframework/act-morphia)
* [act-beetlsql plugin 1.1.0](https://github.com/actframework/act-beetlsql)
* [act-mustache plugin 1.0.0](https://github.com/actframework/act-mustache)
* [Undertow 1.4.8.Final](http://undertow.io/)

## Local Test Result

**Note** the data below is outdated

|            Test            | Throughput |  Latency  | Timeout/Error |
| -------------------------- | ---------: | --------: | ------: |
| json                       | 1026530.41 |    3.31ms |         |
| single query - mongo       |   42185.82 |   17.04ms |         |
| single query - mysql       |   24258.46 |   29.21ms |         |
| single query - pgsql       |   32745.68 |   24.48ms |         |
| 20 queries - mongo         |    2584.76 |   98.92ms |         |
| 20 queries - mysql         |    1317.33 |  191.92ms |         |
| 20 queries - pgsql         |    1966.24 |  128.47ms |         |
| fortunes - mongo           |   23637.79 |   31.12ms |         |
| fortunes - mysql           |   20958.60 |   37.76ms |         |
| fortunes - pgsql           |   23081.28 |   34.31ms |         |
| 20 updates - mongo         |    1239.06 |  202.40ms |         |
| 20 updates - mysql         |    1319.04 |  189.94ms |      71 |
| 20 updates - pgsql         |     803.26 |  312.14ms |         |
| plaintext                  | 1376490.27 |    2.49ms |         |


### Local Test Logs

#### JSON Serialization

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /json 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.11ms    2.90ms 207.19ms   89.55%
    Req/Sec   258.94k    53.37k  465.94k    69.25%
  Latency Distribution
     50%    2.70ms
     75%    4.18ms
     90%    5.82ms
     99%   10.55ms
  10310688 requests in 10.04s, 1.65GB read
Requests/sec: 1026530.41
Transfer/sec:    168.38MB
```

#### Plain text

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /plaintext 16
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.49ms    2.55ms  34.96ms   89.71%
    Req/Sec   346.49k    84.82k  647.92k    68.50%
  Latency Distribution
     50%    1.80ms
     75%    3.08ms
     90%    5.12ms
     99%   13.25ms
  13840896 requests in 10.06s, 1.96GB read
Requests/sec: 1376490.27
Transfer/sec:    199.53MB
```

#### Single Query - MongoDB

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/db 4
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    17.04ms    6.47ms  58.42ms   63.53%
    Req/Sec    10.64k   546.37    15.19k    78.75%
  Latency Distribution
     50%   16.73ms
     75%   22.25ms
     90%   25.04ms
     99%   33.17ms
  425144 requests in 10.08s, 71.27MB read
Requests/sec:  42185.82
Transfer/sec:      7.07MB
```

#### Single query - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/db 4
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    29.21ms   10.81ms 121.06ms   59.54%
    Req/Sec     6.12k   280.08     8.77k    80.25%
  Latency Distribution
     50%   29.02ms
     75%   38.33ms
     90%   43.44ms
     99%   51.34ms
  243744 requests in 10.05s, 40.86MB read
Requests/sec:  24258.46
Transfer/sec:      4.07MB
```

#### Single query - Postgres

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/db 4
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    24.48ms   13.59ms 140.15ms   72.81%
    Req/Sec     8.25k   742.58    10.67k    83.00%
  Latency Distribution
     50%   21.86ms
     75%   29.66ms
     90%   44.31ms
     99%   69.61ms
  329380 requests in 10.06s, 55.21MB read
Requests/sec:  32745.68
Transfer/sec:      5.49MB
```

#### 20 queries - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/queries?queries=20 1
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    98.92ms   33.48ms 367.17ms   79.73%
    Req/Sec   651.20     43.98     0.92k    75.50%
  Latency Distribution
     50%   93.50ms
     75%  107.41ms
     90%  137.61ms
     99%  222.72ms
  25978 requests in 10.05s, 19.39MB read
Requests/sec:   2584.76
Transfer/sec:      1.93MB
```

#### 20 queries - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/queries?queries=20 1
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   191.92ms   16.38ms 254.96ms   89.13%
    Req/Sec   331.99    140.32   646.00     66.33%
  Latency Distribution
     50%  192.68ms
     75%  199.43ms
     90%  206.16ms
     99%  219.29ms
  13225 requests in 10.04s, 9.87MB read
Requests/sec:   1317.33
Transfer/sec:      0.98MB
```

#### 20 queries - Postgres
 
```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/queries?queries=20 1
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   128.47ms   25.97ms 288.45ms   65.68%
    Req/Sec   496.92     40.72   828.00     78.30%
  Latency Distribution
     50%  123.88ms
     75%  143.43ms
     90%  163.07ms
     99%  201.05ms
  19854 requests in 10.10s, 14.82MB read
Requests/sec:   1966.24
Transfer/sec:      1.47MB
```

#### 20 updates - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/updates?queries=20 1
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   202.40ms   41.73ms 497.85ms   78.06%
    Req/Sec   314.17     57.45   646.00     82.66%
  Latency Distribution
     50%  198.61ms
     75%  218.83ms
     90%  252.33ms
     99%  336.21ms
  12469 requests in 10.06s, 9.31MB read
Requests/sec:   1239.06
Transfer/sec:      0.92MB
```

#### 20 updates - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/updates?queries=20 1
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   189.94ms   19.91ms 320.25ms   78.53%
    Req/Sec   332.16     72.08   600.00     75.69%
  Latency Distribution
     50%  187.81ms
     75%  199.80ms
     90%  213.79ms
     99%  243.72ms
  13237 requests in 10.04s, 9.84MB read
  Non-2xx or 3xx responses: 71
Requests/sec:   1319.04
Transfer/sec:      0.98MB
```

#### 20 updates - Postgresql

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/updates?queries=20 1
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   312.14ms   47.72ms 415.26ms   75.75%
    Req/Sec   214.47    161.95   610.00     59.04%
  Latency Distribution
     50%  321.52ms
     75%  347.91ms
     90%  366.58ms
     99%  396.16ms
  8061 requests in 10.04s, 6.02MB read
Requests/sec:    803.26
Transfer/sec:    613.87KB
```

#### Fortunes - Mongo

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mongo/fortunes 4
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    31.12ms   14.03ms 183.18ms   72.31%
    Req/Sec     5.97k   478.08     8.68k    71.75%
  Latency Distribution
     50%   29.62ms
     75%   39.61ms
     90%   46.90ms
     99%   76.91ms
  238284 requests in 10.08s, 331.78MB read
Requests/sec:  23637.79
Transfer/sec:     32.91MB
```

#### Fortunes - MySQL

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /mysql/fortunes 4
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    37.76ms   21.11ms 220.85ms   78.62%
    Req/Sec     5.28k   365.21     6.75k    71.50%
  Latency Distribution
     50%   33.95ms
     75%   45.70ms
     90%   62.66ms
     99%  116.81ms
  210548 requests in 10.05s, 293.16MB read
Requests/sec:  20958.60
Transfer/sec:     29.18MB
```

#### Fortunes - Postgresql

```
luog@luog-Satellite-P50-A:~/p/greenlaw110/FrameworkBenchmarks/frameworks/Java/act$ wrk -t4 -c256 -d10s http://localhost:8080 -s pipeline.lua --latency -- /pgsql/fortunes 4
Running 10s test @ http://localhost:8080
  4 threads and 256 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    34.31ms   18.68ms 179.08ms   71.53%
    Req/Sec     5.83k   385.75    10.12k    80.50%
  Latency Distribution
     50%   31.53ms
     75%   41.27ms
     90%   61.99ms
     99%   96.27ms
  232596 requests in 10.08s, 323.86MB read
Requests/sec:  23081.28
Transfer/sec:     32.14MB
```
