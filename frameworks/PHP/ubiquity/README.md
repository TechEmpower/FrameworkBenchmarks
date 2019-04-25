# Ubiquity Benchmarking Test

## Test Type Implementation Source Code
The tests are separated into 4 controllers:
- `Json` for JSON response
  * [JSON](app/controllers/Json.php)
- `Db` for database access
  * [DB](app/controllers/Db.php)
  * [QUERY](app/controllers/Db.php)
  * [CACHED QUERY (not implemented)]()
  * [UPDATE](app/controllers/Db.php)
- `Fortunes` for using the Twig template engine
  * [FORTUNES](app/controllers/Fortunes.php)
- `Plaintext` for plaintext response
  * [PLAINTEXT](app/controllers/Plaintext.php)


## Important Libraries
The tests were run with:
* [Ubiquity 2.1.2](https://ubiquity.kobject.net/)
* [PHP Version 7.3.*](http://www.php.net/) with FPM and APC
* [nginx 1.14](http://nginx.org/)
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
