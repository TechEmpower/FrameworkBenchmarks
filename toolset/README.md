# Benchmark Tools

This directory contains tools and instructions that you can use to deploy and run the benchmark suite on an environment under your control.

## Deployment

For deployment instructions, refer to the [Benchmark Suite Deployment README file](deployment/README.md).

## Running the test suite on Linux

Assuming the deployment finished without error, we're ready to start the test suite. On the Linux server host, type:

	nohup toolset/run-tests.py -s server-private-ip -d database-private-ip -c client-private-ip -i path-to-private-ssh-key -u username --max-threads number-of-cores --duration 15 &

For the `number-of-cores` parameter, you will need to know your application server's core count. For example, Amazon EC2 large instances have 2 cores.

`path-to-private-ssh-key` is the pathname of the private SSH key file of the Linux client's user. This file is set up during deployment.  `username` is the username associated with the private SSH key.

This script will run the full set of tests. Be aware that on Large instances, if you include the slower frameworks (and they are included by default), **the total runtime of a full suite of tests can be measured in days**, not just hours. The EC2 bill isn't going to break the bank, but it's also not going to be chump change.

The test results are saved in the `results` directory, as described below.

## Running the test suite on Windows

To run the test suite using Windows as the application server and Linux as the database server, log on to the server, open the PowerShell command line environment and type a command like:

	python toolset/run-tests.py -s server-private-ip -d database-private-ip -c client-private-ip -i path-to-private-ssh-key -u username --max-threads 8 --duration 15 --os windows --sleep 5 --name win

To run the test suite using Windows as the application server and Windows as the database server, use this instead:

	python toolset/run-tests.py -s server-private-ip -d database-private-ip -c client-private-ip -i path-to-private-ssh-key -u username --max-threads 8 --duration 15 --os windows --database-os windows --name win
    
## Specifying the machine name

If you use a different configuration than two m1.large instances, please use the --name option to name the results appropriately.

	nohup toolset/run-tests.py -s server-private-ip -d database-private-ip -c client-private-ip -u username -i path-to-private-ssh-key --max-threads cores --name ec2-servertype-clienttype &

So if you were running an m1.large and an m1.medium, it would look like this:

	nohup toolset/run-tests.py -s server-private-ip -d database-private-ip -c client-private-ip -u username -i path-to-private-ssh-key --max-threads cores --name ec2-m1.large-m1.medium &

This will allow us to differentiate results.

## Benchmarking a Single Test

If you are making changes to any of the tests, or you simply want to verify a single test, you can run the script with the --test flag. For example, if you only wanted to run the JRuby tests:

	nohup toolset/run-tests.py -s server-private-ip -d database-private-ip -c client-private-ip -u username -i path-to-private-ssh-key --max-threads cores --name unique-machine-name --test rack-jruby sinatra-jruby rails-jruby

## Result Files

After a test run, the directory `~/FrameworkBenchmarks/results/machine-name/timestamp` will contain all the result files.

In this folder there is a directory for each of the test types, each containing the raw results for each framework.

There is also a single `results.json` file that contains all the results as well as some additional information. This file is what we use to drive our blog post, and may or may not be useful to you.

## Additional information

Use `toolset/run-tests.py --help` to see other options that `run-tests.py` recognizes.

## Support

For assistance deploying and running the benchmarks, use our [Google Group](https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks).
