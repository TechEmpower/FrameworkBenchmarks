<p align="center"><a href="https://openswoole.com" target="_blank"><img src="https://openswoole.com/images/swoole-logo.svg#gh-light-mode-only" width="200" /></a></p>


<p align="center"><a href="https://github.com/openswoole/ext-openswoole/actions?query=workflow%3Alib-openswoole"><img src="https://github.com/openswoole/ext-openswoole/workflows/lib-openswoole/badge.svg" alt="lib-openswoole" style="max-width: 100%;"></a>
<a href="https://github.com/openswoole/ext-openswoole/actions?query=workflow%3Aext-openswoole"><img src="https://github.com/openswoole/ext-openswoole/workflows/ext-openswoole/badge.svg" alt="ext-openswoole" style="max-width: 100%;"></a>
<a href="https://github.com/openswoole/ext-openswoole/actions?query=workflow%3Atest-linux"><img src="https://github.com/openswoole/ext-openswoole/workflows/test-linux/badge.svg" alt="test-linux" style="max-width: 100%;"></a>
<a href="https://scan.coverity.com/projects/open-swoole-src" rel="nofollow"><img src="https://camo.githubusercontent.com/74ce2aa24f7fc272064e7afeec3712e0e548cda19202c4af7e42e7cacf2e7f6f/68747470733a2f2f7363616e2e636f7665726974792e636f6d2f70726f6a656374732f32333937302f62616467652e737667" alt="Coverity Scan Build Status" data-canonical-src="https://scan.coverity.com/projects/23970/badge.svg" style="max-width: 100%;"></a></p>

## Open Swoole

Open Swoole is a programmatic server for PHP with async IO, coroutines and fibers: secure, reliable, high performance

+ __Website__: <https://openswoole.com>
+ __Twitter__: <https://twitter.com/openswoole>
+ __Slack__: <https://goo.gl/forms/wooTTDmhbu30x4qC3>
+ __Discord__: <https://discord.gg/5QC57RNPpw>
+ __IDE Helper__: <https://github.com/openswoole/ide-helper>

## Documentation

Documentation for Open Swoole can be found on the [Open Swoole website](https://openswoole.com/docs).

## Installation

> Open Swoole always provides the most reliable stability and the most powerful features in **the latest released version**. Please ensure as much as possible that you are using the latest version.

### 1. Install or upgrade Open Swoole from multiple distribution channels

Please check [Open Swoole Installation Guide](https://openswoole.com/docs/get-started/installation) about how to install Open Swoole on Ubuntu/CentOS/Windows WSL from Docker, PECL or Binary releases channels.

### 2. Compile from source

#### Compiling requirements

+ Linux, OS X or Cygwin, WSL
+ PHP 7.4.0 or later (The higher the version, the better the performance.)
+ GCC 4.8 or later

Download the source packages from [Releases](https://github.com/openswoole/ext-openswoole/releases) or:

```shell
git clone https://github.com/openswoole/ext-openswoole.git && \
cd ext-openswoole
git checkout v22.0.0
phpize && \
./configure && \
make && make install
```

You can find how to fix [Common Installation Errors](https://openswoole.com/docs/get-started/common-install-errors) if there are errors in the installation.

#### Compile configurations

> for example: `./configure --enable-openssl --enable-sockets`

+ `--enable-openssl` or `--with-openssl-dir=DIR`
+ `--enable-sockets`
+ `--enable-http2`
+ `--enable-mysqlnd` (need mysqlnd, it just for supporting `$mysql->escape` method)
+ `--enable-hook-curl`
+ `--with-postgres[=DIR]`

#### Enable Open Swoole extension

After compiling and installing the openswoole extension, you have to add a new line `extension=openswoole.so` at the end of `php.ini` or create a ini file at `conf.d` folder to enable Open Swoole. It is recommended to be added after all the other extensions because openswoole may depend on extensions: sockets, mysqlnd, curl etc.

## Frameworks & Components

> PR are welcome if your framework is using openswoole
 
 - [**Laravel Octane**](https://laravel.com/docs/9.x/octane) Laravel Octane supercharges your application's performance by serving your application using high-powered application servers.
 - [**PHP Runtime**](https://github.com/php-runtime) make it easy to run any kind of PHP Application (Symfony, Laravel, PSR7, Native) with all kinds of Runtimes like OpenSwoole, Bref, Google Cloud Functions, Roadrunner and React PHP with minimal configuration.
 - [**Mezzio Swoole**](https://docs.mezzio.dev/mezzio-swoole/) allows you to run Mezzio and [PSR-15](https://www.php-fig.org/psr/psr-15/) applications on Open Swoole.

## For Contributors

If you like to involve the maintenance of this repo, it is better to get started by submitting PR, you will be invited to the dev group once there are significant contributions. Or join Slack group firstly, the team will provide mentoring and internal support to help you get started.

* [Report issues and feedback](https://github.com/openswoole/ext-openswoole/issues)
* Submit fixes, features via Pull Request

This project exists thanks to all the historical [[Contributors](https://github.com/openswoole/ext-openswoole/graphs/contributors)].

## Security issues

Security issues should be reported privately, via email, to the Open Swoole develop team [hello@openswoole.com](mailto:hello@openswoole.com). You should receive a response within 24 hours. If for some reason you do not, please follow up via email to ensure we received your original message.

## License

Open Swoole is open-sourced software licensed under the [Apache 2.0 license](http://www.apache.org/licenses/LICENSE-2.0.html).
