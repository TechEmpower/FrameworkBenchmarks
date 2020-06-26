# Ubiquity Benchmarking Test
![img](https://github.com/phpMv/ubiquity/blob/master/Banner/banner.png?raw=true)

Ubiquity is a full-stack php framework, These tests involve:
- the ORM part (Full)
- the JSON serialization (native php)

Tests are available with NginX server, Swoole and Workerman platforms.

## Test Type Implementation Source Code
The tests are separated into controllers:
### Ubiquity + PDO Mysql
- `Json` for JSON response
  * [JSON](app/controllers/Json.php)
- `Db` for database access with ORM (PDO Mysql)
  * [DB](app/controllers/Db.php)
  * [QUERY](app/controllers/Db.php)
  * [CACHED QUERY (not implemented)]()
  * [UPDATE](app/controllers/Db.php) Utilizes transactions
- `Fortunes` for using the internal template engine
  * [FORTUNES](app/controllers/Fortunes.php)
- `Plaintext` for plaintext response
  * [PLAINTEXT](app/controllers/Plaintext.php)
### Ubiquity Swoole + Mysql
- `SwooleDbMy` for database access with PDO Mysql driver (update test only)
  * [SwooleDbAsync](app/controllers/SwooleDbAsync.php)
- `SwooleDbAsync` for database access with Swoole coroutine Mysql driver
  * [SwooleDbAsync](app/controllers/SwooleDbAsync.php)
- `SwooleFortunesAsync` used with Swoole coroutine Mysql driver
  * [SwooleFortunes](app/controllers/SwooleFortunes.php)
### Ubiquity Swoole + PostgreSQL
- `SwooleDb` for database access with PDO pgsql driver
  * [SwooleDb](app/controllers/SwooleDb.php)
- `SwooleFortunes` used with PDO pgsql driver
  * [SwooleFortunes](app/controllers/SwooleFortunes.php)
### Ubiquity Workerman + PDO pgsql
- `Workerman` with PDO pgsql driver
  * [WorkerDb](app/controllers/WorkerDb.php)
- `WorkerFortunes` used with Workerman
  * [WorkerFortunes](app/controllers/WorkerFortunes.php)

## Important Libraries
The tests were run with:
* [Ubiquity 2.3.*](https://ubiquity.kobject.net/)
* [PHP Version 7.4.*](http://www.php.net/) with FPM and APC
* [nginx 1.14](http://nginx.org/)
* [Swoole](https://www.swoole.com/), [Ubiquity-swoole](https://github.com/phpMv/ubiquity-swoole)
* [Workerman](https://github.com/walkor/Workerman), [Ubiquity-workerman](https://github.com/phpMv/ubiquity-workerman)
* [MySQL 8.0](https://dev.mysql.com/)


## Test URLs
### JSON

http://localhost:8080/Json

### PLAINTEXT

http://localhost:8080/Plaintext

### DB

http://localhost:8080/Db

### QUERY

- http://localhost:8080/Db/query/
- http://localhost:8080/SwooleDb/query/
- http://localhost:8080/WorkerDb/query/

### CACHED QUERY



### UPDATE

- http://localhost:8080/Db/update/
- http://localhost:8080/SwooleDb/update/
- http://localhost:8080/WorkerDb/update/

### FORTUNES

- http://localhost:8080/Fortunes
- http://localhost:8080/SwooleFortunes
- http://localhost:8080/WorkerFortunes
