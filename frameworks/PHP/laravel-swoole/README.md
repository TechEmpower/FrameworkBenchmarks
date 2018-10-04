# laravel-swoole Benchmarking Test

This is a benchmark test of Laravel running on Swoole.

Swoole is an asynchronous PHP webserver that runs as a PECL extension to PHP.  

Traditional PHP servers use php-fpm to run PHP software.  On each request, php-fpm initializes a new instance of the php framework, processes the request, returns the response, and terminates the php framework.  
This results in decreased performance relative to other technologies like Java or node.js based frameworks which intialize only once and then process multiple requests without terminating in between.

Swoole provides this capability to PHP.  When Swoole starts, it initializes the framework once and handles requests without terminating the framework between requests.  Also, like nginx, netty, node.js, Swoole is an asynchronous event-loop based server.

Laravel-swoole is an adapter layer between Swoole and Laravel.  It provides facades for http requests and PDO database connections.  It launches Laravel worker instances for each cpu core to handle incoming requests. 

Also because Laravel was written under php-fpm environment where the framework is reset between each request, sometimes state changes are not re-initialized between requests since it isn't necessary in an environment where the framework is terminated after each request.
To handle this, Laravel-swoole creates a sandbox for each request with a copy of initial framework state so that any changes made by the request do not impact the state of other incoming requests.

Brion Finlay 10/3/2018  

### Test Type Implementation Source Code

* [JSON](Relative/Path/To/Your/Source/File)
* [PLAINTEXT](Relative/Path/To/Your/Source/File)
* [DB](Relative/Path/To/Your/Source/File)
* [QUERY](Relative/Path/To/Your/Source/File)
* [UPDATE](Relative/Path/To/Your/Source/File)
* [FORTUNES](Relative/Path/To/Your/Source/File)

## Important Libraries
The tests were run with:
* [Swoole](https://www.swoole.co.uk/)
* [laravel-swoole](https://github.com/swooletw/laravel-swoole/wiki)
* [Laravel](https://laravel.com/)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries/[count]

### UPDATE

http://localhost:8080/updates/[count]

### FORTUNES

http://localhost:8080/fortunes
