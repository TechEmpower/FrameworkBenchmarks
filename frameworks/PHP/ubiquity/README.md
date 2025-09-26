# Ubiquity Benchmarking Test
![img](https://github.com/phpMv/ubiquity/blob/master/Banner/banner-duck.png?raw=true)

Ubiquity is a full-stack php framework, These tests involve:
- the ORM part (Full)
- the JSON serialization (native php)

Tests are available with NginX server, Swoole ,Roadrunner, ngx_php and Workerman platforms.

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

### Ubiquity with async platforms (Swoole, Workerman, ngx_php or Roadrunner) + Pgsql
- `Db_` for database access with ORM (PDO Pgsql)
  * [DB](app/controllers/Db_.php)
  * [QUERY](app/controllers/Db_.php)
  * [CACHED QUERY (only with Workerman)](app/controllers/Cache.php)
  * [UPDATE](app/controllers/Db_.php)
- `Fortunes_` for using the internal template engine
  * [FORTUNES](app/controllers/Fortunes_.php)

### Ubiquity with async platforms (Swoole, Workerman, ngx_php or Roadrunner)
- `Json_` for JSON response
  * [JSON](app/controllers/Json_.php)
- `Plaintext_` for plaintext response
  * [PLAINTEXT](app/controllers/Plaintext_.php)

### Ubiquity Workerman + MongoDb
- `DbMongo` for database access
  * [DB](app/controllers/DbMongo.php)
  * [QUERY](app/controllers/DbMongo.php)
  * [UPDATE](app/controllers/DbMongo.php)
- `FortunesMongo` for using the internal template engine
  * [FORTUNES](app/controllers/FortunesMongo.php)


## Important Libraries
The tests were run with:
* [Ubiquity 2.4.*](https://ubiquity.kobject.net/)
* [PHP Version 8.0.*](http://www.php.net/) with FPM
* [nginx](http://nginx.org/)
* [Swoole](https://www.swoole.com/), [Ubiquity-swoole](https://github.com/phpMv/ubiquity-swoole)
* [Workerman](https://github.com/walkor/Workerman), [Ubiquity-workerman](https://github.com/phpMv/ubiquity-workerman)
* [Roadrunner](https://github.com/spiral/roadrunner), [Roadrunner-ubiquity](https://github.com/Lapinskas/roadrunner-ubiquity)
* [ngx_php7](https://github.com/rryqszq4/ngx_php7), [ubiquity-ngx](https://github.com/phpmv/ubiquity-ngx)
* [MySQL 8.0](https://dev.mysql.com/)
* [MongoDb 4.2](https://www.mongodb.com/)


## Test URLs
### JSON

- http://localhost:8080/Json
- http://localhost:8080/Json_

### PLAINTEXT

- http://localhost:8080/Plaintext
- http://localhost:8080/Plaintext_

### DB

- http://localhost:8080/Db
- http://localhost:8080/Db_
- http://localhost:8080/DbMy
- http://localhost:8080/DbMongo/


### QUERY

- http://localhost:8080/Db/query/
- http://localhost:8080/Db_/query/
- http://localhost:8080/DbMy/query/
- http://localhost:8080/DbMongo/query/


### CACHED QUERY

- http://localhost:8080/Cache/cachedQuery/


### UPDATE

- http://localhost:8080/Db/update/
- http://localhost:8080/Db_/update/
- http://localhost:8080/DbMy/update/
- http://localhost:8080/DbMongo/update/


### FORTUNES

- http://localhost:8080/Fortunes
- http://localhost:8080/Fortunes_
- http://localhost:8080/FortunesMongo
