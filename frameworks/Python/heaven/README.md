# Congratulations!

You have successfully built a new test in the suite!

There are some remaining tasks to do before you are ready to open a pull request, however.

## Next Steps

1. Gather your source code.

You will need to ensure that your source code is beneath this directory. The most common solution is to include a `src` directory and place your source code there.

2. Edit `benchmark_config.json`

You will need alter `benchmark_config.json` to have the appropriate end-points and port specified.

3. Create `heaven.dockerfile`

This is the dockerfile that is built into a docker image and run when a benchmark test is run. Specifically, this file tells the suite how to build and start your test application.

You can create multiple implementations and they will all conform to `[name in benchmark_config.json].dockerfile`. For example, the `default` implementation in `benchmark_config.json` will be `heaven.dockerfile`, but if you wanted to make another implementation that did only the database tests for MySQL, you could make `heaven-mysql.dockerfile` and have an entry in your `benchmark_config.json` for `heaven-mysql`.

4. Test your application

        $ tfb --mode verify --test heaven

This will run the suite in `verify` mode for your test. This means that no benchmarks will be captured and we will test that we can hit your implementation end-points specified by `benchmark_config.json` and that the response is correct.

Once you are able to successfully run your test through our suite in this way **and** your test passes our validation, you may move on to the next step.

5. Add your test to `.github/workflows/build.yml`

Edit `.github/workflows/build.yml` to ensure that Github Actions will automatically run our verification tests against your new test. This file is kept in alphabetical order, so find where `TESTDIR=Python/heaven` should be inserted under `env > matrix` and put it there.

6. Fix this `README.md` and open a pull request

Starting on line 49 is your actual `README.md` that will sit with your test implementation. Update all the dummy values to their correct values so that when people visit your test in our Github repository, they will be greated with information on how your test implementation works and where to look for useful source code.

After you have the real `README.md` file in place, delete everything above line 59 and you are ready to open a pull request.

Thanks and Cheers!




# Heaven Benchmarking Test

This is the Heaven Web Framework implementation of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to Heaven Web Framework. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description

[**Heaven**](https://github.com/rayattack/heaven) is a simple, quick to learn, extremely fast (high-performance) framework for building Web Applications with Python 3.6+.

The key features are:

* **Fast**: Very high performance, on par with **NodeJS** and **Go** (thanks to ASGI).

* **Fast to code**: Increase the speed to develop features by about 300% to 500% *.
* **Less bugs**: Reduce about 40% of human (developer) induced errors. *
* **Easy**: Designed to be easy to use and learn. Less time reading docs.
* **Short**: Minimize code duplication. Multiple features from each parameter declaration. Less bugs.
* **Robust**: Get production-ready code. With automatic interactive documentation.
* **Loosely Opinionated**: You choose if you want to Pydantic, UJson or Orjson, SQLAlchemy or AsyncPG - Heaven get's out of your way *very fast*.

<small>* estimation based on tests on an internal development team, building production applications.</small>

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

All the tests are based on the ones for ASGI, as Heaven is an ASGI based framework.


## Resources

* [Heaven source code on GitHub](https://github.com/rayattack/heaven)
* [Heaven website - documentation](https://rayattack.github.io/heaven/)
