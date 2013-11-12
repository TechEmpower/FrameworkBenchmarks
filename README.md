# Web Framework Performance Comparison

This project provides representative performance measures across a wide field of web application frameworks. With much help from the community, coverage is quite broad and we are happy to broaden it further with contributions. The project presently includes frameworks on many languages including Go, Python, Java, Ruby, PHP, Clojure, Groovy, JavaScript, Erlang, Haskell, Scala, Lua, and C.  The current tests exercise plaintext responses, JSON seralization, database reads and writes via the object-relational mapper (ORM), collections, sorting, server-side templates, and XSS counter-measures.  Future tests will exercise other components and greater computation.

Read more and see the results of our tests on Amazon EC2 and physical hardware at http://www.techempower.com/benchmarks/

Join in the conversation at our Google Group: https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks

## Running the test suite

We ran our tests using two dedicated i7 2600k machines as well as two EC2 m1.large instances.

On the [Benchmark Tools README file](toolset/README.md) you will find tools and instructions to replicate our tests using EC2, Windows Azure or your own dedicated machines.

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
	# start(args, logfile)
	#
	# Starts the server for Wicket
	# returns 0 if everything completes, 1 otherwise
	##################################################
	def start(args, logfile):

    # setting the database url
    setup_util.replace_text("wicket/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

    # 1. Compile and package
    # 2. Clean out possible old tests
    # 3. Copy package to Resin's webapp directory
    # 4. Start resin
    try:
      subprocess.check_call("mvn clean compile war:war", shell=True, cwd="wicket", stderr=logfile, stdout=logfile)
      subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=logfile, stdout=logfile)
      subprocess.check_call("cp wicket/target/hellowicket-1.0-SNAPSHOT.war $RESIN_HOME/webapps/wicket.war", shell=True, stderr=logfile, stdout=logfile)
      subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=logfile, stdout=logfile)
      return 0
    except subprocess.CalledProcessError:
      return 1

	##################################################
	# stop(logfile)
	#
	# Stops the server for Wicket
	# returns 0 if everything completes, 1 otherwise
	##################################################
	def stop(logfile):
    try:
      subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=logfile, stdout=logfile)
      return 0
    except subprocess.CalledProcessError:
      return 1
