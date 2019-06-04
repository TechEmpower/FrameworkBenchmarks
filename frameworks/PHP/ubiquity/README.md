# Ubiquity Benchmarking Test
![img](https://github.com/phpMv/ubiquity/blob/master/Banner/banner.png?raw=true)

Ubiquity is a full-stack php framework, These tests involve:
- the ORM part (Full)
- the JSON serialization (native php)

Tests are available with NginX and PHP-PM servers.

## Test Type Implementation Source Code
The tests are separated into 4 controllers:
- `Json` for JSON response
  * [JSON](app/controllers/Json.php)
- `Db` for database access
  * [DB](app/controllers/Db.php)
  * [QUERY](app/controllers/Db.php)
  * [CACHED QUERY (not implemented)]()
  * [UPDATE](app/controllers/Db.php) Utilizes transactions
- `Fortunes` for using the internal template engine
  * [FORTUNES](app/controllers/Fortunes.php)
- `Plaintext` for plaintext response
  * [PLAINTEXT](app/controllers/Plaintext.php)


## Important Libraries
The tests were run with:
* [Ubiquity 2.1.*](https://ubiquity.kobject.net/)
* [PHP Version 7.3.*](http://www.php.net/) with FPM and APC
* [nginx 1.14](http://nginx.org/)
* [PHP-PM](https://github.com/php-pm/php-pm)
* [Ubiquity-reactphp server](https://github.com/phpMv/ubiquity-reactphp)
* [Ubiquity-php-pm bridge](https://github.com/phpMv/ubiquity-php-pm)
* [MySQL 5.7](https://dev.mysql.com/)

## Servers
PHP-PM server (beta version) is configured with this values:
- workers: 32
- max-requests: 1024

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
