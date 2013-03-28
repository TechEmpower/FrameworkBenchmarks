# Web Framework Benchmarking Tests

Guesses and anecdotes can dominate discussions about the performance of web application frameworks.  Here we attempt to provide some objective performance measures across a wide field of frameworks, covering several platforms: Go, Python, Java, Ruby, PHP, Clojure, Groovy, and JavaScript.  The tests exercise the frameworks' JSON seralization and object-relational model (ORM).  Future versions will exercise server-side template libraries and other computation.

See results data we've collected from Amazon EC2 instances and our physical hardware at our blog. http://www.techempower.com/blog/2013/03/28/framework-benchmarks/

## Running the test suite

We ran our tests using two dedicated i7 2600k machines as well as two EC2 m1.large instances. Below you will find instructions on how to replicate our tests using either EC2 or your own dedicated machines.

###EC2 Instructions

#### 1. Create EC2 Instances

Create two EC2 instances running Ubuntu Server 12.04.1 LTS 64-bit. We tested on m1.large instances, but feel free to experiment with different configurations. Give the instance that will act as the application server more then the default 8GB of disk capacity (we used 20GB).

##### Security Group

When propmted to create a security group for the instances, here are the ports that you'll need to open.

* 22 (SSH)
* 8080 (Most of the tests run on 8080)
* 3306 (MySQL)
* 9000 (Play Framework)
* 27017 (MongoDB)

#### 2. Setting up the servers

To coordinate the tests via scripting, the servers need to be able to work together. So once the instances are running, the first thing you'll want to do is copy your ssh key to the application server instance so that you can ssh between the two machines:

	sftp -i path-to-pem-file ubuntu@server-instance-ip
	put path-to-pem-file .ssh/
	exit

Now ssh into the server instance and clone the latest from this repository (the scripts we use to run the tests expect that you'll clone the repository into your home directory):

	ssh -i path-to-pem-file ubuntu@server-instance-ip
	sudo apt-get install git-core
	git clone https://github.com/TechEmpower/FrameworkBenchmarks.git
	cd FrameworkBenchmarks

Next, we're going to setup the servers with all the necessary software:

	./run-tests.py -s server-private-ip -c client-private-ip -i path-to-pem --install-software --list-tests
    source ~/.bash_profile
    # Most software is installed autormatically by the script, but running the mongo command below from 
    # the install script was causing some errors. For now this needs to be run manually.
    go get github.com/hoisie/web
	mongo --host client-private-ip < config/create.js

Assuming the above finished without error, we're ready to start the test suite:

	nohup ./run-tests.py -s server-private-ip -c client-private-ip -i path-to-pem --max-threads number-of-cores &

For the number-of-cores parameter, you will need to know your application server's core count. For example, Amazon EC2 large instances have 2 cores.

This script will run the full set of tests. Results of all the tests will output to ~/FrameworkBenchmarks/results/ec2/*timestamp*. If you use a different configuration than two m1.large instances, please use the --name option to name the results appropriately.

	nohup ./run-tests.py -s server-private-ip -c client-private-ip -i path-to-pem --max-threads cores --name ec2-servertype-clienttype &

So if you were running an m1.large and an m1.medium, it would look like this:

	nohup ./run-tests.py -s server-private-ip -c client-private-ip -i path-to-pem --max-threads cores --name ec2-m1.large-m1.medium &

This will allow us to differentiate results.

Be aware that on Large instances, if you include the slower frameworks (and they are included by default), the total runtime of a full suite of tests can be measured in days, not just hours. The EC2 bill isn't going to break the bank, but it's also not going to be chump change.

### Dedicated Hardware Instructions

If you have two servers or workstations lying around, then you can install and run the tests on physical hardware. Please be aware that these setup instructions can overwrite software and settings, It's best to follow these instructions on clean hardware. We assume that both machines are running Ubuntu Server 12.04 64-bit.

#### 1. Prerequisites

Before you get started, there are a couple of steps you can take to make running the tests easier on yourself. Since the tests can run for several hours, it helps to set everything up so that once the tests are running, you can leave the machines unattended and don't need to be around to enter ssh or sudo passwords.

1. Setup an ssh key for the client machine
2. Edit your sudoers file so that you do not need to enter your password for sudo access

#### 2. Setting up the servers

As it currently stands, the script that runs the tests makes some assumptions about where the code is placed, we assume that the FrameworkBenchmarks repository will be located in your home directory.

Check out the latest from github:

	cd ~
	git clone https://github.com/TechEmpower/FrameworkBenchmarks.git
	cd FrameworkBenchmarks

Next, we're going to setup the servers with all the necessary software:

	./run-tests.py -s server-ip -c client-ip -i path-to-ssh-key --install-software --list-tests
    source ~/.bash_profile
    # Most software is installed autormatically by the script, but running the mongo command below from
    # the install script was causing some errors. For now this needs to be run manually.
    go get github.com/hoisie/web
	mongo --host client-ip < config/create.js

Assuming this finished without error, we're ready to start the test suite:

	nohup ./run-tests.py -s server-ip -c client-ip -i path-to-ssh-key --max-threads cores --name unique-machine-name &

This will run the full set of tests. Results of all the tests will output to ~/FrameworkBenchmarks/results/unique-machine-name/*timestamp*.

## Result Files

After a test run, the directory ~/FrameworkBenchmarks/results/machine-name/timestamp will contains all the result files. In this folder are four files: three CSV files, one for each of the test types (json, db, query), and a single results.json file that contains all the results as well as some additional information. The results.json file is what we use to drive our blog post, and may or may not be useful to you. There are three subdirectories: one for each of the test types (json, db, query), each of these directories contain the raw weighttp results for each framework.

## Benchmarking a Single Test

If you are making changes to any of the tests, or you simply want to verify a single test, you can run the script with the --test flag. For example, if you only wanted to run the JRuby tests:

	nohup ./run-tests.py -s server-ip -c client-ip -i path-to-ssh-key --max-threads cores --name unique-machine-name --test rack-jruby sinatra-jruby rails-jruby

## Updating Tests

We hope that the community will help us in making these tests better, so if you'd like to make any changes to the tests we currently have, here are some things to keep in mind.

### Updating Dependencies

If you're updating a dependency of a framework that uses a dependency management system (Bundler, npm, etc.), please be specific with the version number that you are updating to.

Also, if you do change the dependency of any test, please update the README file for that test to reflect that change, we want to try and keep the README files as up to date as possible.

### Updating Software

If you would like to update any of the software used, again, please be as specific as possible, while we still install some software via apt-get and don't specify a version, we would like to have as much control over the versions as possible.

The main file that installs all the software is in installer.py. It's broken up into two sections, server software and client software.

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

By convnetion, if the test does not use an ORM, and instead uses the raw database connectivity provided by the platform (e.g., JDBC), we append a "-raw" to the test name in the [benchmark_config](#the-benchmark_config-file) file.  For example, "php-raw".

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
    ./run-test.py --next-sort

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