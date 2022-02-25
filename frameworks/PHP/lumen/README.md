# Lumen PHP Framework

[![Build Status](https://travis-ci.org/laravel/lumen-framework.svg)](https://travis-ci.org/laravel/lumen-framework)
[![Total Downloads](https://poser.pugx.org/laravel/lumen-framework/d/total.svg)](https://packagist.org/packages/laravel/lumen-framework)
[![Latest Stable Version](https://poser.pugx.org/laravel/lumen-framework/v/stable.svg)](https://packagist.org/packages/laravel/lumen-framework)
[![Latest Unstable Version](https://poser.pugx.org/laravel/lumen-framework/v/unstable.svg)](https://packagist.org/packages/laravel/lumen-framework)
[![License](https://poser.pugx.org/laravel/lumen-framework/license.svg)](https://packagist.org/packages/laravel/lumen-framework)

Laravel Lumen is a stunningly fast PHP micro-framework for building web applications with expressive, elegant syntax. We believe development must be an enjoyable, creative experience to be truly fulfilling. Lumen attempts to take the pain out of development by easing common tasks used in the majority of web projects, such as routing, database abstraction, queueing, and caching.

## Official Documentation

Documentation for the framework can be found on the [Lumen website](http://lumen.laravel.com/docs).

## Security Vulnerabilities

If you discover a security vulnerability within Lumen, please send an e-mail to Taylor Otwell at taylor@laravel.com. All security vulnerabilities will be promptly addressed.

## License

The Lumen framework is open-sourced software licensed under the [MIT license](http://opensource.org/licenses/MIT)

# lumen-swoole Benchmarking Test

The lumen-swoole test is a benchmark test of Lumen running on Swoole.

Swoole is an asynchronous PHP webserver that runs as a PECL extension to PHP.  

Traditional PHP servers use php-fpm to run PHP software.  On each request, php-fpm initializes a new instance of the php framework, processes the request, returns the response, and terminates the php framework.  
This results in decreased performance relative to other technologies like Java or node.js based frameworks which intialize only once and then process multiple requests without terminating in between.

Swoole provides this capability to PHP.  When Swoole starts, it initializes the framework once and handles requests without terminating the framework between requests.  Also, like nginx, netty, node.js, Swoole is an asynchronous event-loop based server.

Laravel-swoole is an adapter layer between Swoole and Laravel/Lumen.  It provides facades for http requests and PDO database connections.  It launches Laravel worker instances for each cpu core to handle incoming requests. 

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
* [Lumen](https://lumen.laravel.com/)

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


# Add laravel-s Benchmarking Test
[laravel-s](https://github.com/hhxsv5/laravel-s) is an out-of-the-box adapter between Swoole and Laravel/Lumen, similar to laravel-swoole.
