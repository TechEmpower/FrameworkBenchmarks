# Ruby Frameworks

The information below contains information specific to Ruby. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/).

## Infrastructure Software Versions

* [RVM 2.2.1](https://rvm.io/) (Unless tests are run in Travis-CI)

## Adding a New Ruby Framework

### Installation

TFB uses rvm wherever possible to help ruby-based or
jruby-based frameworks setup their environment. 

When verifying the tests in Travis-CI we rely on Travis-CI's 
RVM installation (and $HOME isn't /home/travis while running 
Travis-CI), so we have certain specific caveats to keep 
Travis-CI happy.

#### Install RVM

Most install.sh files will at least have this:

    #!/bin/bash

    fw_depends rvm # This installs RVM

    if [ "$TRAVIS" = "true" ]
    then
      rvmsudo rvm install ruby-2.0.0-p0
    else
      rvm install ruby-2.0.0-p0
    fi

### Set Up with RVM

At the top of your framework's `setup.sh`, put this (if 
you're using RVM): 

    #!/bin/bash

    # Assume single-user installation
    if [ "$TRAVIS" = "true" ]
    then
      source /home/travis/.rvm/scripts/rvm
    else
      source $HOME/.rvm/scripts/rvm
    fi

Because TFB uses Python's `subprocess` module, which runs 
all shell processes in a non-login mode, you must source the 
`rvm` script before using `rvm` anywhere. 

For compatibility with how the framework rounds are executed, 
you must use a single-user installation if you wish to run 
ruby-based TFB tests.

## Get Help

### Ruby Experts

_No experts listed, yet. If you're an expert, add yourself!_

### [Ruby Community](https://www.ruby-lang.org/en/community/)

* `#ruby-lang` on IRC ([irc.freenode.net](https://freenode.net/))
