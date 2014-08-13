# Web Framework Performance Comparison
[![Build Status](https://travis-ci.org/TechEmpower/FrameworkBenchmarks.svg?branch=master)](https://travis-ci.org/TechEmpower/FrameworkBenchmarks)

This project provides representative performance measures across a wide field of web 
application frameworks. With much help from the community, coverage is quite broad and 
we are happy to broaden it further with contributions. The project presently includes 
frameworks on many languages including `Go`, `Python`, `Java`, `Ruby`, `PHP`, `Clojure`, 
`Groovy`, `JavaScript`, `Erlang`, `Haskell`, `Scala`, `Lua`, `C`, and others.  The 
current tests exercise plaintext responses, JSON seralization, database reads 
and writes via the object-relational mapper (ORM), collections, sorting, server-side templates,
and XSS counter-measures. Future tests will exercise other components and greater computation.

[Read more and see the results of our tests on Amazon EC2 and physical hardware](http://www.techempower.com/benchmarks/). 
For descriptions of the test types that we run, see the [test requirements section](http://www.techempower.com/benchmarks/#section=code).

Join in the conversation at our 
[Google Group](https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks), 
or chat with us on [Freenode](https://freenode.net/faq.shtml#whatwhy) at `#techempower-fwbm`. 

# How do I contribute updates or new frameworks?

If you plan to add a new framework or update an existing framework, you only 
need to run in *verify* mode. This will launch the framework, query all URLs, and verify that 
returned data matches the [benchmark requirements](http://www.techempower.com/benchmarks/#section=code). 

If *verify* mode is all you need, you can potentially develop without ever running our 
codebase locally - our [Travis-CI.org](https://travis-ci.org/TechEmpower/FrameworkBenchmarks) setup 
can run the verification for you. This is as simple as going to travis-ci.org, using the 
`Log in with Github` button, and enabling Travis-CI for your fork. All commit pushes 
will be automatically verified by Travis-CI, and you will get full log output. 
You can submit a pull request when your code passes the Travis-CI verification and have 
confidence it will be merged quickly. 
While this development route it slightly slower than standard local development, it does not 
require any local setup process.

You can also run *verify* mode on a single computer, although you should be comfortable with us 
installing multiple large software suites. 
You will need to enable passwordless SSH access to localhost ([google](http://hortonworks.com/kb/generating-ssh-keys-for-passwordless-login/) [for](http://superuser.com/questions/336226/how-to-ssh-to-localhost-without-password) [help](https://help.ubuntu.com/community/SSH/OpenSSH/Keys)), and you will also 
need to enable passwordless sudo (google for help)
Once you have cloned our repository, run `toolset/run-tests.py --help` for detailed 
help on running in *verify* mode and see the sections below for more guidance. 

# How do I run the benchmark myself? 

If you plan to run the benchmark and compare results, you need to run in *benchmark* 
mode. We recommend having a minimum of three distinct computers with a fast network 
connection in between, all of which you should be comfortable installing a large amount
of additional software on. One of these computers (the `app server`) must have passwordless
SSH access to the other two ([google](http://hortonworks.com/kb/generating-ssh-keys-for-passwordless-login/)
[for](http://superuser.com/questions/336226/how-to-ssh-to-localhost-without-password) 
[help](https://help.ubuntu.com/community/SSH/OpenSSH/Keys)), and on every computer 
you will need to have passwordless sudo access (google for help).
Once you have cloned our repository, run `toolset/run-tests.py --help` for detailed 
help on running in *benchmark* mode and see the sections below for more guidance. 

If you are not an expert, please ensure your setup can run in *verify* mode before 
attempting to run in *benchmark* mode. 

# Project Overview 

Running the full benchmark requires at least three computers:

* `app server` : The computer that your framework will be launched on
* `load server`: The computer that will generate client load. Aka `client machine`
* `DB server`  : The computer that runs all the databases

This codebase (aka `TechEmpower/FrameworkBenchmarks` aka `TFB`) must be run on 
the `app server`. The codebase contains a number of `framework directories`, each 
of which contains one or more `framework tests`. While our current setup has 
many directories, we are working to consolidate related code into fewer directories  
with more tests per directory. 

When run, `TFB` will: 
* select which framework tests are to be run based on passed arguments
* install the necessary software (both on the `app server` and other servers)
* launch the framework 
* access the urls listed in [the requirements](http://www.techempower.com/benchmarks/#section=code) and verify the responses
* launch the load generation software on the `load server`
* gather the results
* halt the framework

# Installation and Usage Details

If you choose to run TFB on your own computer, you will need to install 
passwordless SSH to your `load server` and your `database server` from 
your `app server`. You will also need to enable passwordless sudo access
on all three servers. If you are only planning to use *verify* mode, then
all three servers can be the same computer, and you will need to ensure
you have passwordless sudo access to `localhost`. 

We run all tests on [Ubuntu 14.04](http://www.ubuntu.com/download/server), so 
it is recommended you use this for development or use. 

## Configuration File Usage

TFB takes a large number of command line flags, and it can become tedious to 
specify them repeatedly. We recommend you create a `benchmark.cfg` file by 
copying the example `benchmark.cfg.example` file and modifying as needed. 
See `toolset/run-tests.py --help` for a description of each flag. 

For running in *verify* mode, you can set the various hosts to `localhost`. 
For running in *benchmark* mode, you will need the public IP addresses.

Note: environment variables can also be used for a number of the arguments

## Installation Basics

After you have a configuration file, run the following to setup your 
various servers. We use the `--install-only` flag in our examples to 
prevent launching tests at this stage. 

**Setting up the `load server`**

```
toolset/run-tests.py --install client --verbose  --install-only
```

**Setting up the `database server`**

```
toolset/run-tests.py --install database --verbose --install-only
# We are still working to automate MongoDO. Until this, please run
# this as well (replacing database-ip with your own value)
mongo --host database-ip < config/create.js
```

**Setting up the `app server`**

You can choose to selectively install components by using the 
`--test` and `--exclude` flags. 

```
# Install just the software for beego
toolset/run-tests.py --install server --test beego --verbose --install-only

# Install all php software but php-fuel
toolset/run-tests.py --install server --test php* --exclude php-fuel --verbose --install-only

# Install *all* framework software. Expect this to take hours!
# If running on a remote server, use `screen` or `tmux` or `nohup` to 
# prevent the installation from being terminated if you are disconnected
toolset/run-tests.py --install server --verbose --install-only
```

## Listing Tests

You can easily list all available tests

```
╰─$ toolset/run-tests.py --list-tests
activeweb
activeweb-raw
aspnet
aspnet-jsonnet
aspnet-mongodb-raw
aspnet-mono
aspnet-mono-jsonnet
aspnet-mono-mongodb-raw
<snip>
```

## Running Tests

There are a number of options that can be specified: 

```
# Run a verification for test beego
toolset/run-tests.py --test beego --mode verify

# Run the default benchmark for the beego test
toolset/run-tests.py --test beego

# Modify which test types are run during benchmark
toolset/run-tests.py --test beego --type json
toolset/run-tests.py --test beego --type db
toolset/run-tests.py --test beego --type fortune

# Modify a number of options for how the load is generated
toolset/run-tests.py --test beego --max-concurrency 24 --max-threads 24 --duration 20 --max-queries 200

# Run a tiny benchmark
toolset/run-tests.py --test beego --max-threads 2 --max-concurrency 2 
```

## Finding output logs

Logs file locations use the format `results/ec2/latest/logs/wt/err.txt`. 
The general structure is `results/<run name>/<timestamp>/logs/<test name>/<file>`
You can use the `--name` flag to change the `<run name>`
If you re-run the same test multiple times, you will get a different folder
for each `<timestamp>`, although the `latest` folder will be kept up to date. 
The `<test name>` is simply the test you ran, and `<file>` is either `out.txt`
or `err.txt` (these are the `logout` and `logerr` arguments passed into each 
`setup.py` file. 


Note: If you're looking for logs from our official benchmark rounds, see 
[Round 9](https://github.com/TechEmpower/TFB-Round-9) and 
[Round 8](https://github.com/TechEmpower/TFB-Round-8)


# Contribution Guidelines

The community has consistently helped in making these tests better, and we welcome any 
and all changes. These guidelines prevent us from having to give repeated feedback on 
the same topics: 

* **Use specific versions**: If you're updating any software or dependency, please be 
specific with the version number. Also, update the appropriate `README` to reflect that change
* **Rope in experts**: If you're making a performance tweak, our team may not be 
able to verify your code--we are not experts in every language. It's always helpful 
to ping expert users and provide a basic introduction on their credentials. If you 
are an expert that is willing to be pinged occasionally, please add yourself to 
the appropriate test README files. 
* **Use a personal Travis-CI account**: This one is mainly for your own sanity. Our 
[main Travis-CI](https://travis-ci.org/TechEmpower/FrameworkBenchmarks) can occasionally
become clogged with so many pull requests that it takes a day to finish all the 
builds. If you create a fork and enable Travis-CI.org, you will get your own 
build queue. This means 1) only your commits/branches are being verified, so there is 
no delay waiting for an unrelated pull request, and 2) you can cancel unneeded items. 
This does not affect our own Travis-CI setup at all - any commits added to a pull 
request will be verifed as normal. 
* **Read the README**: We know that's cliche. However, our toolset drags in a lot of 
different concepts and frameworks, and it can really help to read the README's, such 
as this one, the one inside the `toolset/` directory, and the ones inside specific 
framework directories

---

# Adding Frameworks or Tests

When adding a new framework or new test to an existing 
framework, please follow these steps:

* Update/add a [benchmark_config](#the-benchmark_config-file)
* Update/add a [setup file](#setup-files)
* Update/add an [install.sh file](#install-file)
* (Optional) Update/add a [bash_profile.sh file](#bash-environment-file)
* When creating a database test, use the table/collection `hello_world`. 
  Our database setup scripts are stored inside the `config/` folder if 
  you need to see the database schema

### The benchmark_config File

The `benchmark_config` file is used by our scripts to identify 
available tests - it should exist at the root of the framework directory.
We

Here is an example `benchmark_config` from the `Compojure` framework. 
There are two different tests listed for the `Compojure` framework, 

    {
      "framework": "compojure",
      "tests": [{
        "default": {
          "setup_file": "setup",
          "json_url": "/compojure/json",
          "db_url": "/compojure/db/1",
          "query_url": "/compojure/db/",
          "fortune_url": "/compojure/fortune-hiccup",
          "plaintext_url": "/compojure/plaintext",
          "port": 8080,
          "approach": "Realistic",
          "classification": "Micro",
          "database": "MySQL",
          "framework": "compojure",
          "language": "Clojure",
          "orm": "Micro",
          "platform": "Servlet",
          "webserver": "Resin",
          "os": "Linux",
          "database_os": "Linux",
          "display_name": "compojure",
          "notes": "",
          "versus": "servlet"
        },
        "raw": {
          "setup_file": "setup",
          "db_url": "/compojure/dbraw/1",
          "query_url": "/compojure/dbraw/",
          "port": 8080,
          "approach": "Realistic",
          "classification": "Micro",
          "database": "MySQL",
          "framework": "compojure",
          "language": "Clojure",
          "orm": "Raw",
          "platform": "Servlet",
          "webserver": "Resin",
          "os": "Linux",
          "database_os": "Linux",
          "display_name": "compojure-raw",
          "notes": "",
          "versus": "servlet"
        }
      }]
    }

* `framework:` Specifies the framework name.
* `tests:` A list of tests that can be run for this framework. In many cases, 
this contains a single element for the "default" test, but additional tests 
can be specified.  Each test name must be unique when concatenated with the
framework name. Each test will be run separately in our Rounds, so it is to your
benefit to provide multiple variations in case one works better in some cases
  * `setup_file:` The location of the [python setup file](#setup-files) that 
  can start and stop the test, excluding the `.py` ending. If your different 
  tests require different setup approachs, use another setup file. 
  * `json_url (optional):` The URI to the JSON test, typically `/json`
  * `db_url (optional):` The URI to the database test, typically `/db`
  * `query_url (optional):` The URI to the variable query test. The URI 
  must be set up so that an integer can be applied to the end of the URI to
  specify the number of queries to run.  For example, `/query?queries=` 
  (to yield `/query?queries=20`) or `/query/` (to yield `/query/20`)
  * `fortune_url (optional):` the URI to the fortunes test, typically `/fortune`
  * `update_url (optional):` the URI to the updates test, setup in a 
  manner similar to `query_url` described above.
  * `plaintext_url (optional):` the URI of the plaintext test, 
  typically `/plaintext`
  * `port:` The port the server is listening on
  * `approach (metadata):` `Realistic` or `Stripped` (see 
  [here](http://www.techempower.com/benchmarks/#section=code&hw=peak&test=json) for a description of all metadata attributes)
  * `classification (metadata):` `Full`, `Micro`, or `Platform`
  * `database (metadata):` `MySQL`, `Postgres`, `MongoDB`, `SQLServer`, or `None`
  * `framework (metadata):` name of the framework
  * `language (metadata):` name of the language
  * `orm (metadata):` `Full`, `Micro`, or `Raw`
  * `platform (metadata):` name of the platform
  * `webserver (metadata):` name of the web-server (also referred 
  to as the "front-end server")
  * `os (metadata):` The application server's operating system, 
  `Linux` or `Windows`
  * `database_os (metadata):` The database server's operating 
  system, `Linux` or `Windows`
  * `display_name (metadata):` How to render this test permutation's name on
  the results web site.  Some permutation names can be really long, 
  so the display_name is provided in order to provide something more succinct.
  * `versus (optional):` The name of another test (elsewhere in this project) that is a subset of this framework.  This allows for the generation of the framework efficiency chart in the results web site.  For example, Compojure is compared to "servlet" since Compojure is built on the Servlets platform.

The requirements section 
[here](http://www.techempower.com/benchmarks/#section=code&hw=peak&test=json) 
explains the expected response for each URL as well all metadata 
options available. 

### Testing on both Windows and Linux

If your framework and platform can execute on both Windows and Linux, we 
encourage you to specify tests for both operating systems.  This increases the 
amount of testing you should do before submitting your pull-request, however, 
so we understand if you start with just one of the two. Travis-CI cannot 
automatically verify Windows-based tests, and therefore you should verify 
your code manually. 

The steps involved are:

* Assuming you have implemented the Linux test already, add a new test 
permutation to your `benchmark_config` file for the Windows test.  When 
the benchmark script runs on Linux, it skips tests where `os` in `Windows`
and vice versa. 
* Add the necessary tweaks to your [setup file](#setup-files) to start and stop on the new operating system.  See, for example, [the script for Go](https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/go/setup.py).
* Test on Windows and Linux to make sure everything works as expected.

### Install File

The `install.sh` file for each framework starts the bash process which will 
install that framework. Typically, the first thing done is to call `fw_depends` 
to run installations for any necessary software that TFB has already 
created installation scripts for. TFB provides a reasonably wide range of 
core software, so your `install.sh` may only need to call `fw_depends` and 
exit. Note: `fw_depends` does not guarantee dependency installation, so 
list software in the proper order e.g. if `foo` depends on `bar`
use `fw_depends bar foo`.

Here are some example `install.sh` files

```bash
#!/bin/bash

# My framework only needs nodejs
fw_depends nodejs
```

```bash
#!/bin/bash

# My framework needs nodejs and mono and go
fw_depends nodejs mono go
```

```bash
#!/bin/bash

# My framework needs nodejs
fw_depends nodejs

# ...and some other software that there is no installer script for.
# Note: Use IROOT variable to put software in the right folder. 
#       You can also use FWROOT to refer to the project root, or 
#       TROOT to refer to the root of your framework
# Please see guidelines on writing installation scripts
wget mystuff.tar.gz -O mystuff.tar.gz
untar mystuff.tar.gz
cd mystuff
make --prefix=$IROOT && sudo make install
```

To see what TFB provides installations for, look in `toolset/setup/linux`
in the folders `frameworks`, `languages`, `systools`, and `webservers`. 
You should pass the filename, without the ".sh" extension, to fw_depends. 
Here is a listing as of July 2014: 

```bash
$ ls frameworks                                                                
grails.sh  nawak.sh  play1.sh  siena.sh     vertx.sh  yesod.sh
jester.sh  onion.sh  play2.sh  treefrog.sh  wt.sh
$ ls languages
composer.sh  erlang.sh   hhvm.sh   mono.sh    perl.sh     pypy.sh     racket.sh   urweb.sh
dart.sh      go.sh       java.sh   nimrod.sh  phalcon.sh  python2.sh  ringojs.sh  xsp.sh
elixir.sh    haskell.sh  jruby.sh  nodejs.sh  php.sh      python3.sh  ruby.sh     yaf.sh
$ ls systools
leiningen.sh  maven.sh
$ ls webservers
lapis.sh  mongrel2.sh  nginx.sh  openresty.sh  resin.sh  weber.sh  zeromq.sh
```

### Bash Environment File

The `bash_profile.sh` file is sourced before installing software or before
running the framework test. This is mostly used when running your 
framework, to perform actions such as updating `PATH` or defining environment 
variables your framework requires e.g. `GOROOT`. You can use these 
variables: 

* **FWROOT:** Root of project
* **IROOT:** Root of installation for the current framework
* **TROOT:** Root directory for the current framework 

Example of `bash_profile.sh`: 

```bash
# Set the root of our go installation
export GOROOT=${IROOT}/go

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

export GOPATH=${FWROOT}/go
```

Do not cause any output, such as using `echo`, inside of `bash_profile.sh`

### Setup Files

The setup file is responsible for starting and stopping the test. This script is responsible for (among other things):

* Modifying the framework's configuration to point to the correct database host
* Compiling and/or packaging the code
* Starting the server
* Stopping the server

The setup file is a python script that contains a start() and a stop() function.  The start function should build the source, make any necessary changes to the framework's configuration, and then start the server.  The stop function should shutdown the server, including all sub-processes as applicable.

#### Configuring database connectivity in start()

By convention, the configuration files used by a framework should specify the database server as `localhost` so that developing tests in a single-machine environment can be done in an ad hoc fashion, without using the benchmark scripts.

When running a benchmark script, the script needs to modify each framework's configuration so that the framework connects to a database host provided as a command line argument.  In order to do this, use setup_util.replace_text() to make necessary modifications prior to starting the server.

For example:

```python
setup_util.replace_text("wicket/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")
```

Using `localhost` in the raw configuration file is not a requirement as long as the `replace_text` call properly injects the database host provided to the benchmarker toolset as a command line argument.

#### A full example

Here is an example of Wicket's setup file.

```python
import subprocess
import sys
import setup_util

##################################################
# start(args, logfile, errfile)
#
# Starts the server for Wicket
# returns 0 if everything completes, 1 otherwise
##################################################
def start(args, logfile, errfile):

# setting the database url
setup_util.replace_text("wicket/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

# 1. Compile and package
# 2. Clean out possible old tests
# 3. Copy package to Resin's webapp directory
# 4. Start resin
try:
  subprocess.check_call("mvn clean compile war:war", shell=True, cwd="wicket", stderr=errfile, stdout=logfile)
  subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=errfile, stdout=logfile)
  subprocess.check_call("cp wicket/target/hellowicket-1.0-SNAPSHOT.war $RESIN_HOME/webapps/wicket.war", shell=True, stderr=errfile, stdout=logfile)
  subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=errfile, stdout=logfile)
  return 0
except subprocess.CalledProcessError:
  return 1

##################################################
# stop(logfile, errfile)
#
# Stops the server for Wicket
# returns 0 if everything completes, 1 otherwise
##################################################
def stop(logfile):
try:
  subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=errfile, stdout=logfile)
  return 0
except subprocess.CalledProcessError:
  return 1
```
      
#### A tool to generate your setup file
 
A contributor named [@kpacha](https://github.com/kpacha) has built a pure JavaScript tool for generating the `setup.py` file for a new framework via an in-browser form.  Check out his [FrameworkBenchmarks Setup Builder](http://kpacha.github.io/FrameworkBenchmarks-setup-builder/).

---

## Windows server setup

* Connect to the Windows server via Remote Desktop.
* Copy `installer-bootstrap.ps1` from "toolset/setup/windows" to the server (use CTRL-C and CTRL-V).
* Copy your Linux client private key too.
* Right click on the installer script and select `Run with PowerShell`.
* Press Enter to confirm.
* It will install git and then launch `installer.ps1` from the repository, which will install everything else.
* The installation takes about 20 minutes.
* Then you have a working console: try `python`, `git`, `ssh`, `curl`, `node` etc. and verify that everything works + PowerShell goodies.

The client/database machine is still assumed to be a Linux box. You can install just the client software via

    python toolset\run-tests.py -s server-private-ip -c client-private-ip -i "C:\Users\Administrator\Desktop\client.key" --install-software --install client --list-tests

but this step is not required if you already installed the Linux server and client as described above.

Now you can run tests:

    python toolset\run-tests.py -s server-private-ip -c client-private-ip -i "C:\Users\Administrator\Desktop\client.key" --max-threads 2 --duration 30 --sleep 5 --name win --test aspnet --type all

---

## SQL Server setup

* Connect to the SQL Server host via Remote Desktop.
* Run a `Command Prompt` as Administrator.
* Enter this command:

        powershell -ExecutionPolicy Bypass -Command "iex (New-Object Net.WebClient).DownloadString('https://raw.github.com/TechEmpower/FrameworkBenchmarks/master/toolset/setup/sqlserver/setup-sqlserver-bootstrap.ps1')"

* This will configure SQL Server, the Windows Firewall, and populate the database.

Now, when running `run-tests.py`, just add `-d <ip of SQL Server instance>`. This works for the (Windows Server-based) `aspnet-sqlserver-raw` and `aspnet-sqlserver-entityframework` tests.
