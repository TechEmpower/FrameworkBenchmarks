# Ubiquity Benchmarking Test
![img](https://github.com/phpMv/ubiquity/blob/master/Banner/banner.png?raw=true)

Ubiquity is a full-stack php framework, These tests involve:
- the ORM part (Full)
- the JSON serialization (native php)

Tests are available with NginX server, Swoole and Workerman platforms.

## Test Type Implementation Source Code
The tests are separated into 7 controllers:
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
- `SwooleDb` for database access with Swoole coroutine Mysql driver
  * [SwooleDb](app/controllers/SwooleDb.php)
- `Workerman` with Mysqli driver
  * [WorkerDb](app/controllers/WorkerDb.php)
- `AsyncFortunes` used with Swoole or Workerman
  * [AsyncFortunes](app/controllers/AsyncFortunes.php)

## Important Libraries
The tests were run with:
* [Ubiquity 2.3.*](https://ubiquity.kobject.net/)
* [PHP Version 7.3.*](http://www.php.net/) with FPM and APC
* [nginx 1.14](http://nginx.org/)
* [Swoole](https://www.swoole.com/)
* [Ubiquity-swoole](https://github.com/phpMv/ubiquity-swoole)
* [Workerman](https://github.com/walkor/Workerman)
* [Ubiquity-workerman](https://github.com/phpMv/ubiquity-workerman)
* [MySQL 5.7](https://dev.mysql.com/)


## Test URLs
### JSON

http://localhost:8080/Json

### PLAINTEXT

http://localhost:8080/Plaintext

### DB

http://localhost:8080/Db

### QUERY

http://localhost:8080/Db/query/

### CACHED QUERY

http://localhost:8080/Db/query/

### UPDATE

http://localhost:8080/Db/update/

### FORTUNES

http://localhost:8080/Fortunes
