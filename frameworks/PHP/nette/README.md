# Nette Benchmarking Test

_Note: opcache.save_comments needs to be 1 in order for the framework to function properly._

### Test Type Implementation Source Code

* [JSON](app/Presenters/SimplePresenter.php)
* [PLAINTEXT](app/Presenters/SimplePresenter.php)
* [DB](app/Presenters/DatabasePresenter.php)
* [QUERY](app/Presenters/DatabasePresenter.php)
* [UPDATE](app/Presenters/DatabasePresenter.php)
* [FORTUNES](app/Presenters/DatabasePresenter.php)

## Important Libraries
The tests were run with:
* [Nette](https://www.nette.org/)

## Test URLs
### JSON

http://localhost:8080/simple/json

### PLAINTEXT

http://localhost:8080/simple/plaintext

### DB

http://localhost:8080/database/db

### QUERY

http://localhost:8080/database/query?queries=

### UPDATE

http://localhost:8080/database/update?queries=

### FORTUNES

http://localhost:8080/database/fortune
