# Web Framework Performance Comparison

This project provides representative performance measures across a wide field of web application frameworks. With much help from the community, coverage is quite broad and we are happy to broaden it further with contributions. The project presently includes frameworks on many languages including Go, Python, Java, Ruby, PHP, Clojure, Groovy, JavaScript, Erlang, Haskell, Scala, Lua, and C.  The current tests exercise plaintext responses, JSON seralization, database reads and writes via the object-relational mapper (ORM), collections, sorting, server-side templates, and XSS counter-measures.  Future tests will exercise other components and greater computation.

Read more and see the results of our tests on Amazon EC2 and physical hardware at http://www.techempower.com/benchmarks/

Join in the conversation at our Google Group: https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks

## Running the test suite

We ran our tests using two dedicated i7 2600k machines as well as two EC2 m1.large instances.

On the [Toolset README file](toolset/README.md) you will find tools and instructions to replicate our tests using either EC2 or your own dedicated machines.

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

There are three different tests that we currently run:

* JSON Response
* Database (single query)
* Database (multiple query)

The single query database test can be treated as a special case of the multiple query test with the query-count parameter set to 1.

### JSON Response

This test needs to follow the following conventions:

* The message object should be instantiated as a new object for each request.
* The test should use a JSON serializer to render the newly instantiated object to JSON.
* Set the response Content-Type to application/json.
* The response should be {"message": "Hello, World!"}
* White space in the response does not matter.

Pseudo-code:

	obj = { message : "Hello, World!" }
	render json.encode(obj)

### Database (single query)

This test will:

* Access a database table or collection named "World" that is known to contain 10,000 rows/entries.
* Query for a single row from the table or collection using a randomly generated id (the ids range from 1 to 10,000).
* Set the response Content-Type to application/json.
* Serialize the row to JSON and send the resulting string as the response.

By convention, if the test does not use an ORM, and instead uses the raw database connectivity provided by the platform (e.g., JDBC), we append a "-raw" to the test name in the [benchmark_config](#the-benchmark_config-file) file.  For example, "php-raw".

Pseudo-code:

	random_id = random(1, 10000)
	world = World.find(random_id)
	render json.encode(world)

### Database (multiple queries)

This test is very similar to the single query test, and in some cases it will be implemented using the same code. A URL parameter is made available to specify the number of queries to run per request. The response is a list of objects resulting from the queries for random rows.

Pseudo-code:

	number_of_queries = get("queries")
	worlds = []
	for i = 0; i < number_of_queries; i++
        random_id = random(1, 10000)
        worlds[i] = World.find(random_id)
	render json.encode(worlds)

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
