# Goravel Benchmarking Test

[Goravel](https://www.goravel.dev/) is a web application framework with complete functions and excellent scalability. As a starting scaffolding to help Gopher quickly build their own applications.

The framework's design is consistent with [Laravel](https://github.com/laravel/laravel), simplifying the learning curve for PHPers. Kudos to Laravel!

### Test Type Implementation Source Code

* [JSON](src/gin/app/http/controllers/test_controller.go)
* [PLAINTEXT](src/gin/app/http/controllers/test_controller.go)
* [DB](src/gin/app/http/controllers/test_controller.go)
* [QUERY](src/gin/app/http/controllers/test_controller.go)
* [CACHED QUERY](src/gin/app/http/controllers/test_controller.go)
* [UPDATE](src/gin/app/http/controllers/test_controller.go)
* [FORTUNES](src/gin/app/http/controllers/test_controller.go)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?q=

### CACHED QUERY

http://localhost:8080/cached_query?q=

### UPDATE

http://localhost:8080/update?q=

### FORTUNES

http://localhost:8080/fortunes
