Silex, a simple Web Framework
=============================

[![Build Status](https://secure.travis-ci.org/fabpot/Silex.png?branch=master)](http://travis-ci.org/fabpot/Silex)

Silex is a simple web framework to develop simple websites based on
[Symfony2][1] components:


```php
<?php
require_once __DIR__.'/../vendor/autoload.php';

$app = new Silex\Application();

$app->get('/hello/{name}', function ($name) use ($app) {
  return 'Hello '.$app->escape($name);
});

$app->run();
```

Silex works with PHP 5.3.3 or later.

## Installation

The recommended way to install Silex is [through
composer](http://getcomposer.org). Just create a `composer.json` file and
run the `php composer.phar install` command to install it:

    {
        "require": {
            "silex/silex": "1.0.*@dev"
        }
    }

Alternatively, you can download the [`silex.zip`][2] file and extract it.

## More Information

Read the [documentation][3] for more information.

## Tests

To run the test suite, you need [composer](http://getcomposer.org) and
[PHPUnit](https://github.com/sebastianbergmann/phpunit).

    $ php composer.phar install --dev
    $ phpunit

## Community

Check out #silex-php on irc.freenode.net.

## License

Silex is licensed under the MIT license.

[1]: http://symfony.com
[2]: http://silex.sensiolabs.org/download
[3]: http://silex.sensiolabs.org/documentation
