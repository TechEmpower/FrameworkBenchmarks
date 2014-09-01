ClanCats Framework 2.0
======================

[![Build Status](https://travis-ci.org/ClanCats/Framework.svg?branch=master)](https://travis-ci.org/ClanCats/Framework)
[![License](http://img.shields.io/packagist/l/clancats/framework.svg)](https://github.com/ClanCats/Framework)
[![Downloads](http://img.shields.io/packagist/dm/clancats/core.svg)](https://github.com/ClanCats/Framework)


ClanCatsFramework, because your time is precious. HMVC PHP framework.

_This is the Application repository if you like to contribute take a look at the core repository:_ https://github.com/ClanCats/Core

## About CCF

This PHP framework was originally build 2010 as the core of a social Plattform called "ClanCats". In 2012 we decided to split the core and the application apart, so the CCF was born. After developing several application on CCF v1.0, the point has come to rethink the core structure and rewrite the entire thing to a new version that should go open source.

## Why CCF?

There are many brilliant frameworks out there, so why should you use this one? Every Framework has it's own beauty, with CCF we strongly focus on simple usage, high extensibility and a clear structure. Just give a us try you will not be disappointed. 
 
## Installation

Setting up a new instance of CCF2 can be done simply through command, or, in other words, with composer.

Run the following command to create a new project with CCF.

```
$ composer create-project clancats/framework <your project name> --prefer-dist
```

_Composer installed? Read the installation guide here: https://getcomposer.org/download/_

## Requirements

To run this framework please check the following requirements:

 * PHP >= 5.3.9
 * PHP JSON
 * PHP MCrypt
 * PHP Multibyte String
 * Apache with mod_rewrite or Nginx

## Permissions

For some operations ( storage, packtacular etc. ) we may need to grant write permissions in the file system. 

**Storage:** `/CCF/storage/`<br/>
**Packtacular:** `/public/assets/packtacular/`

You can also set these using the `cli` doctor.

```
$ php cli doctor::permissions
```

If you get an error setting the permissions try to run the that command with `sudo`.

---

## Structure

In the new CCF2 folder structure we split some things apart to make deployment much more efficient.<br/>


```
 - CCF/
   - app/                // Your Application 
   - core/               // The CCF Core
   - orbit/              // Orbit ships ( plugins / modules ) 
   - storage/            // Internal file storage for logs, cache etc.
   - vendor/             // Composer vendor

 - boot/
   - environment.php     // The Environment configuration
   - paths.php           // Framework paths configuration

 - cli                   // Command line utility
 - composer.json
 - framework.php
 - phpunit.xml

 - public/
   - index.php           // Web Application public
```

---

## Configuration

There is usually no configuration _required_ to just run the framework (depending on your environment). We do, however, recommend to do some configuration before you start developing your awesome application.

### Boot

The boot configuration allow you change the core behavior of the CCF.

#### Environment

Define how your application detects the environment.<br/>
You can create an entirely new script of your own to return the runtime environment or you can make use of the env detector.

```
/boot/environment.php
```

> Check out the environment docs: [Environment](/docs/application/environment)

#### Paths

Plan on running multiple CCF installations on one machine? Using just a single core? This is no problem because you can modify the CCF paths. 

```
/boot/paths.php
```

You are also free to add new paths. Adding a new element to the array will add the path to runtime and also create a path define:

```
<value> = <value>PATH
test = TESTPATH
```

### main config

You will find an initial **main configuration** file under:

```
/CCF/app/config/main.config.php
```

#### Security Salt

At several points CCF is going to encrypt certain things by employing salt. You should define your own salt to keep your application secure.

You can generate a random salt using the following command:

```
$ php cli doctor::security_key
```

**When using composer to create a new project the salt is generated automatically.**

Otherwise you will find the key under security in the main configuration:

```php
'security' => array(
    'salt' => 'L~7(%(9=@9+8u.Oo4+ysT45fkA4,82',
),
```

#### Path offset

Maybe you would like to run your application from somewhere other than the domain root, e.g. from a subfolder.
*forums*.

```php
// www.yourdomain.com/forums/
'url'	=> array(
    'path'		=> '/forums/',
),
```

> For everything else, check out the main configuration documents: [Configuration](/docs/application/main_configuration/)

---

## Routing

Depending on your system you will need to set something up so that all public requests end on the `public/index.php` file.

CCF ships with an `.htaccess` for apache systems with `mod_rewrite` enabled:

```ini
RewriteEngine On
# RewriteBase /subdir/

# Only Rewrite URL if file not exits
RewriteCond %{REQUEST_FILENAME} !-f
RewriteCond %{REQUEST_FILENAME} !-d

# Default
RewriteRule ^(.*)$ index.php?/$1 [QSA,L]
```
