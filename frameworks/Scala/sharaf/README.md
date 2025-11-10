
# Sharaf Benchmarking Test

[Sharaf](https://sake92.github.io/sharaf/) is a minimalistic Scala 3 web framework.

### Test Type Implementation Source Code

* [JSON](src/routes.scala)
* [PLAINTEXT](src/routes.scala)
* [DB](src/routes.scala)
* [QUERY](src/routes.scala)
* [UPDATE](src/routes.scala)
* [FORTUNES](src/routes.scala)

## Important Libraries
The tests were run with:
* [squery](https://sake92.github.io/squery/) for SQL
* [tupson](https://sake92.github.io/tupson/) for JSON
* [scalatags](https://com-lihaoyi.github.io/scalatags/) for HTML

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=


### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
