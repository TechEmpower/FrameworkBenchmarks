<p align="center"><img src="https://laravel.com/assets/img/components/logo-laravel.svg"></p>

<p align="center">
<a href="https://travis-ci.org/laravel/framework"><img src="https://travis-ci.org/laravel/framework.svg" alt="Build Status"></a>
<a href="https://packagist.org/packages/laravel/framework"><img src="https://poser.pugx.org/laravel/framework/d/total.svg" alt="Total Downloads"></a>
<a href="https://packagist.org/packages/laravel/framework"><img src="https://poser.pugx.org/laravel/framework/v/stable.svg" alt="Latest Stable Version"></a>
<a href="https://packagist.org/packages/laravel/framework"><img src="https://poser.pugx.org/laravel/framework/license.svg" alt="License"></a>
</p>

## About Laravel

Laravel is a web application framework with expressive, elegant syntax. We believe development must be an enjoyable and creative experience to be truly fulfilling. Laravel attempts to take the pain out of development by easing common tasks used in the majority of web projects, such as:

- [Simple, fast routing engine](https://laravel.com/docs/routing).
- [Powerful dependency injection container](https://laravel.com/docs/container).
- Multiple back-ends for [session](https://laravel.com/docs/session) and [cache](https://laravel.com/docs/cache) storage.
- Expressive, intuitive [database ORM](https://laravel.com/docs/eloquent).
- Database agnostic [schema migrations](https://laravel.com/docs/migrations).
- [Robust background job processing](https://laravel.com/docs/queues).
- [Real-time event broadcasting](https://laravel.com/docs/broadcasting).

Laravel is accessible, yet powerful, providing tools needed for large, robust applications.

## Learning Laravel

Laravel has the most extensive and thorough [documentation](https://laravel.com/docs) and video tutorial library of any modern web application framework, making it a breeze to get started learning the framework.

If you're not in the mood to read, [Laracasts](https://laracasts.com) contains over 1100 video tutorials on a range of topics including Laravel, modern PHP, unit testing, JavaScript, and more. Boost the skill level of yourself and your entire team by digging into our comprehensive video library.

## Laravel Sponsors

We would like to extend our thanks to the following sponsors for helping fund on-going Laravel development. If you are interested in becoming a sponsor, please visit the Laravel [Patreon page](https://patreon.com/taylorotwell):

- **[Vehikl](https://vehikl.com/)**
- **[Tighten Co.](https://tighten.co)**
- **[Kirschbaum Development Group](https://kirschbaumdevelopment.com)**
- **[British Software Development](https://www.britishsoftware.co)**
- [Fragrantica](https://www.fragrantica.com)
- [SOFTonSOFA](https://softonsofa.com/)
- [User10](https://user10.com)
- [Soumettre.fr](https://soumettre.fr/)
- [CodeBrisk](https://codebrisk.com)
- [1Forge](https://1forge.com)
- [TECPRESSO](https://tecpresso.co.jp/)
- [Pulse Storm](http://www.pulsestorm.net/)
- [Runtime Converter](http://runtimeconverter.com/)
- [WebL'Agence](https://weblagence.com/)

## Contributing

Thank you for considering contributing to the Laravel framework! The contribution guide can be found in the [Laravel documentation](https://laravel.com/docs/contributions).

## Security Vulnerabilities

If you discover a security vulnerability within Laravel, please send an e-mail to Taylor Otwell via [taylor@laravel.com](mailto:taylor@laravel.com). All security vulnerabilities will be promptly addressed.

## License

The Laravel framework is open-sourced software licensed under the [MIT license](https://opensource.org/licenses/MIT).

# laravel-swoole Benchmarking Test

The laravel-swoole test is a benchmark test of Laravel running on Swoole.

Swoole is an asynchronous PHP webserver that runs as a PECL extension to PHP.  

Traditional PHP servers use php-fpm to run PHP software.  On each request, php-fpm initializes a new instance of the php framework, processes the request, returns the response, and terminates the php framework.  
This results in decreased performance relative to other technologies like Java or node.js based frameworks which intialize only once and then process multiple requests without terminating in between.

Swoole provides this capability to PHP.  When Swoole starts, it initializes the framework once and handles requests without terminating the framework between requests.  Also, like nginx, netty, node.js, Swoole is an asynchronous event-loop based server.

Laravel-swoole is an adapter layer between Swoole and Laravel/Lumen.  It provides facades for http requests and PDO database connections.  It launches Laravel worker instances for each cpu core to handle incoming requests. 

Also because Laravel was written under php-fpm environment where the framework is reset between each request, sometimes state changes are not re-initialized between requests since it isn't necessary in an environment where the framework is terminated after each request.
To handle this, Laravel-swoole creates a sandbox for each request with a copy of initial framework state so that any changes made by the request do not impact the state of other incoming requests.

benchmark support: [Brion Finlay](https://github.com/bfinlay) 10/3/2018

# laravel-roadrunner Benchmarking Test

The laravel-roadrunner test is a benchmark test of Laravel running on [Roadrunner](https://github.com/spiral/roadrunner).

RoadRunner is an open-source (MIT licensed) high-performance PHP application server, load balancer, and process manager. It supports running as a service with the ability to extend its functionality on a per-project basis.

RoadRunner includes PSR-7/PSR-17 compatible HTTP and HTTP/2 server and can be used to replace classic Nginx+FPM setup with much greater performance and flexibility.

RoadRunner achieves performance improvements by reusing PHP instances so that the framework is not bootstrapped on each request.

RoadRunner uses a synchronous model of execution rather than asynchronous for broader compatibility with libraries.

benchmark support: [Brion Finlay](https://github.com/bfinlay) 4/16/2021

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
* [Roadrunner](https://github.com/spiral/roadrunner)
* [Roadrunner Laravel Bridge](https://github.com/spiral/roadrunner-laravel)
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

# Add laravel-s Benchmarking Test
[laravel-s](https://github.com/hhxsv5/laravel-s) is an out-of-the-box adapter between Swoole and Laravel/Lumen, similar to laravel-swoole.
