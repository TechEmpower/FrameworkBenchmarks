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

The benchmark_config file is used by our run script to identify the available tests to be run. This file should exist at the root of the test directory. Here is its basic structure:

	{
      "framework": "my-framework",
      "tests": [{
        "default": {
          "setup_file": "setup.py"
          "json_url": "/json",
          "db_url": "/db",
          "query_url": "/db?queries=",
          "port": 8080,
          "sort": 32
      }, {
        "alternative": {
          "setup_file": "alternate_setup.py"
          "json_url": "/json",
          "db_url": "/db",
          "query_url": "/db?queries=",
          "port": 8080,
          "sort": 33
        }
      }]
	}

* framework: Specifies the framework name.
* tests: An array of tests that can be run for this framework. In most cases, this contains a single element for the "default" test, but additional tests can be specified.
  * setup_file: The location of the [setup file](#setup-files) that can start and stop the test. By convention this is just setup.py.
  * json_url (optional): The relative URL path to the JSON test
  * db_url (optional): The relative URL path to the database test
  * query_url (optional): The relative URL path to the variable query test. The URL must be set up so that an integer can be applied to the end of the url to specify the number of queries to run, i.e. /db?queries= or /db/
  * port: The port the server is listneing on
  * sort: The sort order. This is important for our own blog post which relies on consistent ordering of the frameworks. You can get the next available sort order by running:
    ./run-tests.py --next-sort

## Setup Files

The setup file is responsible for starting and stopping the test. This script is responsible for (among other things):

* Setting the database host to the correct IP
* Compiling/packaging the code
* Starting the server
* Stopping the server

The setup file is a python file that contains a start() and a stop() function. Here is an example of Wicket's setup file.

	import subprocess
	import sys
	import setup_util

	##################################################
	# start(args)
	#
	# Starts the server for Wicket
	# returns 0 if everything completes, 1 otherwise
	##################################################
	def start(args):

    # setting the database url
    setup_util.replace_text("wicket/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

    # 1. Compile and package
    # 2. Clean out possible old tests
    # 3. Copy package to Resin's webapp directory
    # 4. Start resin
    try:
      subprocess.check_call("mvn clean compile war:war", shell=True, cwd="wicket")
      subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
      subprocess.check_call("cp wicket/target/hellowicket-1.0-SNAPSHOT.war $RESIN_HOME/webapps/wicket.war", shell=True)
      subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True)
      return 0
    except subprocess.CalledProcessError:
      return 1

	##################################################
	# stop()
	#
	# Stops the server for Wicket
	# returns 0 if everything completes, 1 otherwise
	##################################################
	def stop():
    try:
      subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True)
      return 0
    except subprocess.CalledProcessError:
      return 1
