# Cake PHP Benchmarking Test

This is the Cake PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](app/Controller/JsonController.php)


### Data-Store/Database Mapping Test
Uses the CakePHP Model functionality.

* [DB test controller](app/Controller/WorldController.php)
* [DB test model](app/Model/World.php)


## Infrastructure Software Versions
The tests were run with:

* [Cake Version 4.0](https://cakephp.org/)
* [PHP Version 7.4](http://www.php.net/)
* [nginx 1.12.0](http://nginx.org/)
* [MySQL 5.7.20](https://dev.mysql.com/)

Cake Debug mode is set to false in [core.php](config/app.php), as
appropriate for a production deployment.

## Test URLs
### JSON Encoding Test

http://localhost/index.php/json

### Data-Store/Database Mapping Test

http://localhost/index.php/db

### Variable Query Test
    
http://localhost/index.php/queries?queries=2

### Variable Update Test

http://localhost/index.php/updates?queries=2

CakePHP
=======

[![CakePHP](http://cakephp.org/img/cake-logo.png)](http://www.cakephp.org)

CakePHP is a rapid development framework for PHP which uses commonly known design patterns like Active Record, Association Data Mapping, Front Controller and MVC.
Our primary goal is to provide a structured framework that enables PHP users at all levels to rapidly develop robust web applications, without any loss to flexibility.

Some Handy Links
----------------

[CakePHP](http://www.cakephp.org) - The rapid development PHP framework

[Cookbook](http://book.cakephp.org) - THE Cake user documentation; start learning here!

[Plugins](http://plugins.cakephp.org/) - A repository of extensions to the framework

[The Bakery](http://bakery.cakephp.org) - Tips, tutorials and articles

[API](http://api.cakephp.org) - A reference to Cake's classes

[CakePHP TV](http://tv.cakephp.org) - Screen casts from events and video tutorials

[The Cake Software Foundation](http://cakefoundation.org/) - promoting development related to CakePHP

Get Support!
------------

[Our Google Group](http://groups.google.com/group/cake-php) - community mailing list and forum

[#cakephp](http://webchat.freenode.net/?channels=#cakephp) on irc.freenode.net - Come chat with us, we have cake.

[Q & A](http://ask.cakephp.org/) - Ask questions here, all questions welcome

[Lighthouse](http://cakephp.lighthouseapp.com/) - Got issues? Please tell us!

[![Bake Status](https://secure.travis-ci.org/cakephp/cakephp.png?branch=master)](http://travis-ci.org/cakephp/cakephp)

![Cake Power](https://raw.github.com/cakephp/cakephp/master/lib/Cake/Console/Templates/skel/webroot/img/cake.power.gif)
