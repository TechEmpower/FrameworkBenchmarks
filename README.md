
# Welcome to [TechEmpower Framework Benchmarks (TFB)](http://www.techempower.com/benchmarks/) 
[![Build Status](https://travis-ci.org/TechEmpower/FrameworkBenchmarks.svg?branch=master)](https://travis-ci.org/TechEmpower/FrameworkBenchmarks) 
[![Documentation Status](https://readthedocs.org/projects/frameworkbenchmarks/badge/?version=latest)](https://readthedocs.org/projects/frameworkbenchmarks/?badge=latest)
[![Issue Stats](http://www.issuestats.com/github/TechEmpower/FrameworkBenchmarks/badge/issue?style=flat)](http://www.issuestats.com/github/TechEmpower/FrameworkBenchmarks)
[![Issue Stats](http://www.issuestats.com/github/TechEmpower/FrameworkBenchmarks/badge/pr?style=flat)](http://www.issuestats.com/github/TechEmpower/FrameworkBenchmarks)

If you're new to the project, welcome! Please feel free to ask questions [here](https://github.com/TechEmpower/FrameworkBenchmarks/issues/2978). We encourage new frameworks and contributors to ask questions. We're here to help!

This project provides representative performance measures across a wide field of web application frameworks. With much help from the community, coverage is quite broad and we are happy to broaden it further with contributions. The project presently includes frameworks on many languages including `Go`, `Python`, `Java`, `Ruby`, `PHP`, `C#`, `Clojure`, `Groovy`, `Dart`, `JavaScript`, `Erlang`, `Haskell`, `Scala`, `Perl`, `Lua`, `C`, and others.  The current tests exercise plaintext responses, JSON seralization, database reads and writes via the object-relational mapper (ORM), collections, sorting, server-side templates, and XSS counter-measures. Future tests will exercise other components and greater computation.

[Read more and see the results of our tests on cloud and physical hardware](http://www.techempower.com/benchmarks/). For descriptions of the test types that we run, see the 
[test requirements section](https://frameworkbenchmarks.readthedocs.org/en/latest/Project-Information/Framework-Tests/).

If you find yourself in a directory or file that you're not sure what the purpose is, checkout our [file structure](http://frameworkbenchmarks.readthedocs.org/en/latest/Codebase/#file-structure) in our documenation, which will briefly explain the use of relevant directories and files.

## Quick Start Guide

To get started developing you'll need to install [docker](https://docs.docker.com/install/) or see our [Quick Start Guide using vagrant](.#quick-start-guide-(vagrant))

1. Clone TFB.

        $ git clone https://github.com/TechEmpower/FrameworkBenchmarks.git

#### A note on Windows:

Git on Windows will, by default, automatically convert line endings from `lf` to `crlf`. This is problematic for the Docker images we build for tests targeting Linux operating systems. Therefore, in order to run tests on Windows, you must clone the repository with `autocrlf` as either `false` or `input`.

See [this writeup](https://help.github.com/articles/dealing-with-line-endings/) for more information.

2. Create the TFB Docker virtual network

        $ docker network create tfb

3. Run a test.

        $ docker run -it --network=tfb -v /var/run/docker.sock:/var/run/docker.sock --mount type=bind,source=[ABS PATH TO THIS DIR],target=/FrameworkBenchmarks techempower/tfb --mode verify --test gemini

### Explanation of the run script

That run script is pretty wordy, but each and every flag is required. Unfortunately, because of the way that Docker runs processes, you **cannot** put this inside of a shell script without breaking how `ctrl+c` and `SIGTERM` work (the shell script would receive the signal, do nothing with the underlying python suite running, and exit, orphaning the toolset to continue running).

- `-it` tells docker to run this in 'interactive' mode and simulate a TTY, so that `ctrl+c` is propagated.
- `--network=tfb` tells the container to join the 'tfb' Docker virtual network
- `-v` specifies which Docker socket path to mount as a volume in the running container. This allows docker commands run inside this container to use the host container's docker to create/run/stop/remove containers.
- `--mount` mounts the FrameworkBenchmarks source directory as a volume to share with the container so that rebuilding the toolset image is unnecessary and any changes you make on the host system are available in the running toolset container.
- `techempower/tfb` is the name of toolset container to run
- `--mode verify --test gemini` are the command to pass to the toolset.

#### A note on Linux:

You may not want to call step 4 from above every time. You can add an `alias` to your `~/.bash_aliases` file to shorten it since it will not change once configured:

`$ alias tfb="docker run -it --network=tfb -v /var/run/docker.sock:/var/run/docker.sock --mount type=bind,source=[ABS PATH TO THIS DIR],target=/FrameworkBenchmarks techempower/tfb"`

`$ source ~/.bash_aliases`

Now you can run the toolset via `tfb`:

`$ tfb --mode verify --test gemini`

#### A note on Windows:

- Docker expects Linux-style paths. If you cloned on your `C:\` drive, then `[ABS PATH TO THIS DIR]` would be `/c/FrameworkBenchmarks`.
- [Docker for Windows](https://www.docker.com/docker-windows) understands `/var/run/docker.sock` even though that is not a valid path on Windows. [Docker Toolbox](https://docs.docker.com/toolbox/toolbox_install_windows/) **may** not - use at your own risk.

## Quick Start Guide (Vagrant)

Get started developing quickly by utilizing vagrant with TFB. [Git](https://git-scm.com), 
[Virtualbox](https://www.virtualbox.org/) and [vagrant](https://www.vagrantup.com/) are 
required.

1. Clone TFB.

        $ git clone https://github.com/TechEmpower/FrameworkBenchmarks.git

2. Change directories

        $ cd FrameworkBenchmarks/deployment/vagrant

3. Build the vagrant virtual machine

        $ vagrant up

4. Run a test

        $ vagrant ssh
        $ tfb --mode verify --test gemini


## Add a New Test

Once you open an SSH connection to your vagrant box, start the new test initialization wizard.

        vagrant@TFB-all:~/FrameworkBenchmarks$ tfb --new

This will walk you through the entire process of creating a new test to include in the suite.


## Resources

#### Official Documentation
Our official documentation can be found at 
[frameworkbenchmarks.readthedocs.org](https://frameworkbenchmarks.readthedocs.org/). 
If you find any errors or areas for improvement within the docs, feel free to either submit a [pull request](https://github.com/TechEmpower/TFB-Documentation/pulls) or [issue](https://github.com/TechEmpower/TFB-Documentation/issues) at the [documentation repository](https://github.com/TechEmpower/TFB-Documentation).

#### Live Results
Results of continuous benchmarking runs are available in real time [here](https://tfb-status.techempower.com/).

#### Data Visualization
If you have a `results.json` file that you would like to visualize, you can [do that here](https://www.techempower.com/benchmarks/#section=test). You can also attach a `runid` parameter to that url where `runid` is a run listed on [tfb-status](https://tfb-status.techempower.com) like so: https://www.techempower.com/benchmarks/#section=test&runid=fd07b64e-47ce-411e-8b9b-b13368e988c6

## Contributing

The community has consistently helped in making these tests better, and we welcome any and all changes. Reviewing our contribution practices and guidelines will help to keep us all on the same page. The [contribution guide](https://frameworkbenchmarks.readthedocs.org/en/latest/Development/Contributing-Guide/) can be found in the [TFB documentation](https://frameworkbenchmarks.readthedocs.org/).

Join in the conversation on our [mailing list](https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks), on [Twitter](https://twitter.com/tfbenchmarks), or chat with us on [Freenode](https://webchat.freenode.net/) at `#techempower-fwbm`. 
