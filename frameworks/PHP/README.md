# PHP Frameworks

[![php logo](https://avatars1.githubusercontent.com/u/25158?s=200&v=4)](https://php.net)

The information below contains information specific to PHP. 
For further guidance, review the 
[documentation](https://frameworkbenchmarks.readthedocs.io/en/latest/).

## Infrastructure Software Versions

### PHP Versions

[PHP 7.3.\*, PHP 5.6.*](http://php.net/) and [HHVM 3.30](https://hhvm.com/).

## Adding New PHP-based Frameworks

### PHP Acceleration and Caching

Caching the output of the PHP bytecode compiler is expressly 
allowed by this benchmark. As we use PHP 5.5, which comes 
with opcache built in, we recommend you use this. However, 
some frameworks utilize APC instead as switching can be 
problematic (e.g. APC allows arbitrary data caching, while 
opcache does not). 

Caching the output of parsing your configuration files is 
also expressly allowed (e.g. file caching, metadata caching).
Some frameworks use APCu or memcached to achieve this. 

Caching of the classloader (often referred to as optimizing
the classloader) is also allowed. Most frameworks have their 
own methods of doing this. 

*Caching of any data fetched from the database is not allowed*. 
Specifically, things such as [Doctrine's Result Cache](http://doctrine-orm.readthedocs.org/en/latest/reference/caching.html#result-cache) are inadmissible. 

However, if you are using an ORM that prepares SQL 
statements in some way, such as how 
[Doctrine](http://doctrine-orm.readthedocs.org/en/latest/reference/caching.html#query-cache) 
translates DQL into SQL, this translated form can be 
cached, as long as you are dynamically accepting 
query parameters. 

Caching any data using databases (Redis, MongoDB, etc) 
is discouraged, as 1) our databases run on a separate 
computer across the network, so you won't see much 
benefit 2) your usage of the DB *may* impact other 
framework's tests, which we cannot allow. You may launch 
a DB locally on the application server as part of your 
`setup.sh` scripts and utilize it for caching the allowable
cache items, if you so desire, but it's normally much 
easier to use systems such as APCu.

Ask if you are not certain.

### Dependency Management Using Composer

Many PHP apps use [Composer](https://getcomposer.org/) for dependency management, 
which greatly simplifies downloading the framework, loading the framework, and 
upgrading the framework version in the future. 

## Get Help

### PHP Experts

_There aren't any PHP experts listed, yet. If you're an expert, 
add yourself!_

### Resources & Interesting Links

_If you find some interesting links related to the PHP tests, 
add them here._
