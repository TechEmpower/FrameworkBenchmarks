# Web Framework Performance Comparison

This project provides representative performance measures across a wide field of web application frameworks. With much help from the community, coverage is quite broad and we are happy to broaden it further with contributions. The project presently includes frameworks on many languages including Go, Python, Java, Ruby, PHP, Clojure, Groovy, JavaScript, Erlang, Haskell, Scala, Lua, and C.  The current tests exercise plaintext responses, JSON seralization, database reads and writes via the object-relational mapper (ORM), collections, sorting, server-side templates, and XSS counter-measures.  Future tests will exercise other components and greater computation.

Read more and see the results of our tests on Amazon EC2 and physical hardware at http://www.techempower.com/benchmarks/

Join in the conversation at our Google Group: https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks

### Prerequisites

Before starting setup, all the required hosts must be provisioned, with the respective operating system and required software installed, and with connectivity for remote management (SSH on Linux, RDP and WinRM on Windows).

Refer to [Benchmark Suite Deployment README file](../deployment/README.md) for the provisioning procedures documentation.

### App, load, and database servers

**NOTE:** If testing a pull request or doing development, it is usually adequate to only use one computer. In that case, your server, client, and database IPs will be 127.0.0.1

#### Installing the Framework Benchmark App Server

* Install [Ubuntu 14.04](http://www.ubuntu.com/download/server) with username `tfb`. Ensure that OpenSSH is selected when you install. If not, run the following command
```bash
$ sudo apt-get install openssh-server
```
* If Ubuntu is already installed, run the following command and follow the prompts.
```bash
$ sudo adduser tfb
```
* Log in as `tfb`
* Fully update **NOTE**: If you update the kernel (linux-firmware), it is generally a good idea to reboot aftewards.
```bash
$ sudo apt-get update && sudo apt-get upgrade
```
* Run the command: `sudo visudo`
* Change line 20 in from `%sudo   ALL=(ALL:ALL) ALL` to `%sudo   ALL=NOPASSWD: ALL`
* Run the following **(Don't enter a password, just hit enter when the prompt pops up)**. **NOTE** This is still necessary if the client and database are on the same computer as the server
```bash
$ ssh-keygen
$ ssh-copy-id <database ip>
$ ssh-copy-id <client ip>
```
* Install git and clone the Framework Benchmarks repository
```bash
$ sudo apt-get install git
$ cd ~
$ git clone https://github.com/TechEmpower/FrameworkBenchmarks.git
$ cd FrameworkBenchmarks
```
* Install the server software. This will take a long time
```bash
$ nohup python toolset/run-tests.py -s <server hostname/ip> -c <client hostname/ip> -u tfb --install-software --install server --list-tests &
```
* If you want to view the process of installing, do the following. The session can be interrupted easily so no need to worry about keeping a connection.
```bash
$ tail -f nohup.out
```
* Reboot when the install is done
* Edit your ~/.bashrc file to change the following
 * Change `TFB_SERVER_HOST=<ip address>` to the server's IP address
 * Change `TFB_CLIENT_HOST=<ip address>` to the client's ip address
 * Change `TFB_DATABASE_HOST=<ip address>` to the database's ip address.
 * Change `TFB_CLIENT_IDENTITY_FILE=<path>` to the id file you specified when you ran ssh-keygen (probably /home/tfb/.ssh/id_rsa if you don't know what it is)
 * Run the command `source ~/.bashrc`
* If you are setting up any other servers, do so before proceeding.
* Run the following commands
```bash
cd ~/FrameworkBenchmarks
source ~/.bash_profile
# For your first time through the tests, set the ulimit for open files
ulimit -n 8192
# Most software is installed automatically by the script, but running the mongo command below from
# the install script was causing some errors. For now this needs to be run manually.
cd installs && curl -sS https://getcomposer.org/installer | php -- --install-dir=bin
cd ..
sudo apt-get remove --purge openjdk-6-jre openjdk-6-jre-headless
# Change database-private-ip to the database ip
mongo --host database-private-ip < config/create.js
```
* Before running the tests, do the following
```bash
$ source ~/.bashrc
```

---

#### Installing the Framework Benchmark Database Server

* Install [Ubuntu 14.04](http://www.ubuntu.com/download/server) with username `tfb`
* Log in as `tfb`
* Fully update **NOTE**: If you update the kernel (linux-firmware), it is generally a good idea to reboot aftewards.
```bash
$ sudo apt-get update && sudo apt-get upgrade
```
* Run the command: `sudo visudo`
* Change line 20 in from `%sudo   ALL=(ALL:ALL) ALL` to `%sudo   ALL=NOPASSWD: ALL`
* On the app server, run the following from the FrameworkBenchmark directory (this should only take a small amount of time, several minutes or so):
```bash
$ toolset/run-tests.py --install-software --install database --list-tests
```

---

#### Installing the Framework Benchmark Load Server

* Install [Ubuntu 14.04](http://www.ubuntu.com/download/server) with username `tfb`
* Log in as `tfb`
* Fully update **NOTE**: If you update the kernel (linux-firmware), it is generally a good idea to reboot aftewards.
```bash
$ sudo apt-get update && sudo apt-get upgrade
```
* Run the command: `sudo visudo`
* Change line 20 in from `%sudo   ALL=(ALL:ALL) ALL` to `%sudo   ALL=NOPASSWD: ALL`
* On the app server, run the following from the FrameworkBenchmark directory (this should only take a small amount of time, several minutes or so):
```bash
$ toolset/run-tests.py --install-software --install client --list-tests
```

You can validate that the setup worked by running a smoke test like this:

    toolset/run-tests.py --max-threads 1 --name smoketest --test servlet-raw --type all -m verify

This should run the verification step for a single framework.

---

#### Windows server setup

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

#### SQL Server setup

* Connect to the SQL Server host via Remote Desktop.
* Run a `Command Prompt` as Administrator.
* Enter this command:

        powershell -ExecutionPolicy Bypass -Command "iex (New-Object Net.WebClient).DownloadString('https://raw.github.com/TechEmpower/FrameworkBenchmarks/master/toolset/setup/sqlserver/setup-sqlserver-bootstrap.ps1')"

* This will configure SQL Server, the Windows Firewall, and populate the database.

Now, when running `run-tests.py`, just add `-d <ip of SQL Server instance>`. This works for the (Windows Server-based) `aspnet-sqlserver-raw` and `aspnet-sqlserver-entityframework` tests.

---

## Running the test suite

We ran our tests using three dedicated i7 2600k machines, three EC2 m1.large instances, and three servers from Peak Hosting

## Updating Tests

We hope that the community will help us in making these tests better, so if you'd like to make any changes to the tests we currently have, here are some things to keep in mind.

### Updating Dependencies

If you're updating a dependency of a framework that uses a dependency management system (Bundler, npm, etc.), please be specific with the version number that you are updating to.

Also, if you do change the dependency of any test, please update the README file for that test to reflect that change, we want to try and keep the README files as up to date as possible.

### Updating Software

If you would like to update any of the software used, again, please be as specific as possible, while we still install some software via apt-get and don't specify a version, we would like to have as much control over the versions as possible.

The main file that installs all the software is in `toolset/setup/linux/installer.py`. It's broken up into two sections, server software and client software.

Additionally, it may be necessary to update the setup.py file in the framework's directory to use this new version.

If you update any software, please update the README files of any tests that use that software.

## Adding Frameworks

When adding a new framework or new test to an existing framework, please follow these steps:

* Update/add [benchmark_config](#the-benchmark_config-file)
* Update/add [setup file](#setup-files)
* When creating a database test, please use the MySQL table hello_world.World, or the MongoDB collection hello_world.world

### The Tests

For descriptions of the test types that we run against each framework, see the [test requirements section of the Results web site](http://www.techempower.com/benchmarks/#section=code).

## The benchmark_config File

The benchmark_config file is used by our scripts to both identify the available tests and to extract metadata describing each test.

This file should exist at the root of the test directory.

Here is the basic structure of benchmark_config, using the Compojure framework as an example.  Compojure has two test *permutations*, which are identified as the "tests" list in the JSON structure below.

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

* framework: Specifies the framework name.
* tests: An list of tests that can be run for this framework. In many cases, this contains a single element for the "default" test, but additional tests can be specified.  Each test name must be unique when concatenated with the framework name.
  * setup_file: The location of the [setup file](#setup-files) that can start and stop the test. By convention this is just setup.py.
  * json_url (optional): The URI to the JSON test, typically `/json`
  * db_url (optional): The URI to the database test, typically `/db`
  * query_url (optional): The URI to the variable query test. The URI must be set up so that an integer can be applied to the end of the URI to specify the number of queries to run.  For example, "/query?queries=" (to yield /query?queries=20" or "/query/" to yield "/query/20".
  * fortune_url (optional): the URI to the fortunes test, typically `/fortune`
  * update_url (optional): the URI to the updates test, setup in a manner similar to the query_url described above.
  * plaintext_url (optional): the URI of the plaintext test, typically `/plaintext`
  * port: The port the server is listening on
  * approach (metadata): `Realistic` or `Stripped` (see results web site for description of all metadata attributes)
  * classification (metadata): `Full`, `Micro`, or `Platform`
  * database (metadata): `MySQL`, `Postgres`, `MongoDB`, `SQLServer`, or `None`
  * framework (metadata): name of the framework
  * language (metadata): name of the language
  * orm (metadata): `Full`, `Micro`, or `Raw`
  * platform (metadata): name of the platform
  * webserver (metadata): name of the web-server (also referred to as the "front-end server")
  * os (metadata): The application server's operating system, `Linux` or `Windows`
  * database_os (metadata): The database server's operating system, `Linux` or `Windows`
  * display_name (metadata): How to render this test permutation's name in the results web site.  Some permutation names can be really long, so the display_name is provided in order to provide something more succinct.
  * versus (optional): The name of another test (elsewhere in this project) that is a subset of this framework.  This allows for the generation of the framework efficiency chart in the results web site.  For example, Compojure is compared to "servlet" since Compojure is built on the Servlets platform.

### Testing on both Windows and Linux

If your framework and platform can execute on both Windows and Linux, we encourage you to specify tests for both operating systems.  This increases the amount of testing you should do before submitting your pull-request, however, so we understand if you start with just one of the two.

The steps involved are:

* Assuming you have implemeneted the Linux test already, add a new test permutation to your `benchmark_config` file for the Windows test (or vice-versa).  When the benchmark script runs on Linux, it skips tests where the Application Operating System (`os` in the file) is specified as Linux.  When running on Windows, it skips tests where the `os` field is Linux.
* Add the necessary tweaks to your [setup file](#setup-files) to start and stop on the new operating system.  See, for example, [the script for Go](https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/go/setup.py).
* Test on Windows and Linux to make sure everything works as expected.

## Setup Files

The setup file is responsible for starting and stopping the test. This script is responsible for (among other things):

* Modifying the framework's configuration to point to the correct database host
* Compiling and/or packaging the code
* Starting the server
* Stopping the server

The setup file is a python script that contains a start() and a stop() function.  The start function should build the source, make any necessary changes to the framework's configuration, and then start the server.  The stop function should shutdown the server, including all sub-processes as applicable.

### Configuring database connectivity in start()

By convention, the configuration files used by a framework should specify the database server as `localhost` so that developing tests in a single-machine environment can be done in an ad hoc fashion, without using the benchmark scripts.

When running a benchmark script, the script needs to modify each framework's configuration so that the framework connects to a database host provided as a command line argument.  In order to do this, use setup_util.replace_text() to make necessary modifications prior to starting the server.

For example:

    setup_util.replace_text("wicket/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

Using `localhost` in the raw configuration file is not a requirement as long as the `replace_text` call properly injects the database host provided to the benchmarker toolset as a command line argument.

### A full example

Here is an example of Wicket's setup file.

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
      
### A tool to generate your setup file ###
 
A contributor named [@kpacha](https://github.com/kpacha) has built a pure JavaScript tool for generating the `setup.py` file for a new framework via an in-browser form.  Check out his [FrameworkBenchmarks Setup Builder](http://kpacha.github.io/FrameworkBenchmarks-setup-builder/).
