# Congratulations!

You have successfully built a new test in the suite!

There are some remaining tasks to do before you are ready to open a pull request, however.

## Next Steps

1. Gather your source code.

You will need to ensure that your source code is beneath this directory. The most common solution is to include a `src` directory and place your source code there.

2. Edit `source_files`

A metric we capture, in addition to the actual benchmark numbers, is the significant lines of code required to run your application. To help our suite identify your source code, we require you to list your source files in `source_files`.

3. Edit `.gitignore`

It is very important that any files created by building or running your application are included in your `.gitignore`. The repository **must** be only source files and the files the suite requires for starting your test application.

4. Edit `benchmark_config.json`

The initialization process made some assumptions about your test implementation that may or may not be true. For example, it laid out two separate tests: the non-database tests; and the database tests. You, on the other hand, may only want to implement the `JSON` test, so you will need alter `benchmark_config.json`.

Additionally, `benchmark_config.json` has, for each test, a key called "setup_file". This value refers to the next bullet.

5. Edit `setup.sh`

This is the script that is executed when a benchmark test is run. Specifically, this file tells the suite how to build and start your test application.

In this file, there are detailed instructions on what is expected and what is available to help you start your test application.

6. Test your application

        $ tfb --mode verify --test $NAME

This will run the suite in `verify` mode for your test. This means that no benchmarks will be captured and we will test that we can hit your implementation end-points specified by `benchmark_config.json` and that the response is correct.

Once you are able to successfully run your test through our suite in this way **and** your test passes our validation, you may move on to the next step.

7. Fix this `README.md` and open a pull request

Starting on line 55 is your actual `README.md` that will sit with your test implementation. Update all the dummy values to their correct values so that when people visit your test in our Github repository, they will be greated with information on how your test implementation works and where to look for useful source code.

After you have the real `README.md` file in place, delete everything above line 55 and you are ready to open a pull request.

Thanks and Cheer!







# $DISPLAY_NAME Benchmarking Test

### Test Type Implementation Source Code

* [JSON](Relative/Path/To/Your/Source/File)
* [PLAINTEXT](Relative/Path/To/Your/Source/File)
* [DB](Relative/Path/To/Your/Source/File)
* [QUERY](Relative/Path/To/Your/Source/File)
* [CACHED QUERY](Relative/Path/To/Your/Source/File)
* [UPDATE](Relative/Path/To/Your/Source/File)
* [FORTUNES](Relative/Path/To/Your/Source/File)

## Important Libraries
The tests were run with:
* [Software](https://www.example1.com/)
* [Example](http://www.example2.com/)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### CACHED QUERY

http://localhost:8080/cached_query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes