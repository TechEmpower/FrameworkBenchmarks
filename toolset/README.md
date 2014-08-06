# TFB Toolset 

This directory contains the code that TFB uses to automate installing, 
launching, load testing, and terminating each framework. 

## Travis Integration

This section details how 
[TFB](https://github.com/TechEmpower/FrameworkBenchmarks) 
integrates with 
[Travis Continuous Integration](https://travis-ci.org/TechEmpower/FrameworkBenchmarks). At a 
high level, there is a github hook that notifies travis-ci every 
time new commits are pushed to master, or every time new commits 
are pushed to a pull request. Each push causes travis to launch a 
virtual machine, checkout the code, run an installation, and run
a verification.

[Travis-ci.org](https://travis-ci.org/) is a free 
([pro available](https://travis-ci.com/)) service, and we have a limited 
number of virtual machines available. If you are pushing one 
commit, consider including `[ci skip]` *anywhere* in the commit 
message if you don't need Travis. If you are pushing many commits, 
use `[ci skip]` in *all* of the commit messages to disable Travis. 

### Travis Terminology

Each push to github triggers a new travis *build*. Each *build* 
contains a number of independent *jobs*. Each *job* is run on an
isolated virtual machine called a *worker*. 

Our project has one *job* for each framework directory, e.g. 
one *job* for `go`, one *job* for `activeweb`, etc. Each 
*job* gets it's own *worker* virtual machine that runs the 
installation for that framework (using `--install server`) and 
verifies the framework's output using `--mode verify`. 

The *.travis.yml* file specifies the *build matrix*, which is 
the set of *jobs* that should be run for each *build*. Our 
*build matrix* lists each framework directory, which causes 
each *build* to have one *job* for each listed directory. 

### Travis Limits

[Travis-ci.org](https://travis-ci.org/) is a free 
([pro available](https://travis-ci.com/)) service, and therefore imposes 
multiple limits. 

Each time someone pushes new commits to master (or to a pull request), 
a new *build* is triggered that contains ~100 *jobs*, one for each 
framework directory. This obviously is resource intensive, so it is 
critical to understand travis limits. 

**Minutes Per Job**: `50 minutes` maxiumum. None of the *job*s we run hit 
this limit (most take 10-15 minutes total)

**Max Concurrent Jobs**: `4 jobs`, but can increase to 10 if Travis has low 
usage. This is our main limiting factor, as each *build* causes ~100 *jobs*.
Discussed below

**Min Console Output**: If `10 minutes` pass with no output to stdout or stderr, 
Travis considers the *job* as errored and halts it. This affects some of our
larger tests that perform part of their installation inside of their `setup.py`. 
Discussed below

**Max Console Output**: A *job* can only ouput `4MB` of log data before it 
is terminated by Travis. Some of our larger builds (e.g. `aspnet`) run into 
this limit, but most do not

### Dealing with Travis' Limits

**Max Concurrent Jobs**: Basically, we cancel any unneeded jobs. Practically,
canceling is entirely handled by `run-ci.py`. If needed, the TechEmpower team
can manually cancel *jobs* (or *builds*) directly from the Travis website. 
Every *build* queues every *job*, there is no way to not queue *jobs*
we don't need, so the only solution is to cancel the unneeded jobs. 

**Min Console Output**: Some frameworks run part of their installation 
inside of their `setup.py`'s start method, meaning that all output goes into 
the `out.txt` file for that test. The TFB toolset needs to be updated to 
occasionally trigger some output, although this is a non-trivial change for a 
few reasons. If your framework is erroring in this way, consider attempting to 
run your installation from the `install.sh` file, which avoids this issue. 

### Advanced Travis Details

#### The Run-Continuous Integration (e.g. run-ci.py) Script

`run-ci.py` is the main script for each *job*. While `run-ci.py` calls 
`run-test.py` to do any actual work, it first checks if there is any 
reason to run a verfication for this framework. This check uses `git diff`
to list the files that have been modified. If files relevant to this 
framwork have not been modified, then `run-ci.py` doesn't bother running 
the installation (or the verification) as nothing will have changed from 
the last build. We call this a **partial verification**, and if only one 
framework has been modified then the entire build will complete within 
10-15 minutes. 

*However, if anything in the `toolset/` directory has been modified, then
every framework is affected and no jobs will be cancelled!* We call this 
a **full verification**, and the entire build will complete within 4-5 hours. 

In order to cancel Travis *jobs*, `run-ci.py` uses the [Travis Command Line
Interface](https://github.com/travis-ci/travis.rb). Only TechEmpower admins
have permission to cancel *jobs* on 
[TechEmpower's Travis Account](https://travis-ci.org/TechEmpower/FrameworkBenchmarks/builds/31771076), 
so `run-ci.py` uses an authentication token to log into Github (and therefore
Travis) as a TechEmpower employee, and then cancels *jobs* as needed. 

#### The 'jobcleaner' 

Because we have so many *jobs*, launching *workers* just to have them be 
cancelled can take quite a while. `jobcleaner` is a special job listed first
in the *build matrix*. `run-ci.py` notices the `jobcleaner` keyword and 
attempts to cancel any unnecessary *jobs* before the *workers* are even 
launched. In practice this is quite effective - without `jobcleaner` a 
partial verification takes >1 hour, but with `jobcleaner` a partial 
verification can take as little as 10 minutes.  

This takes advantage of the fact that Travis currently runs the 
*build matrix* roughly top to bottom, so `jobcleaner` is triggered early 
in the build. 

#### Pull Requests vs Commits To Master

When verifying code from a pull request, `run-ci.py` cannot cancel any 
builds due to a Travis [security restriction](http://docs.travis-ci.com/user/pull-requests/#Security-Restrictions-when-testing-Pull-Requests) 
([more details](https://github.com/TechEmpower/FrameworkBenchmarks/issues/951)). 

Therefore, `run-ci.py` returns `pass` for any *job* that it would normally 
cancel. *Jobs* that would not be canceled are run as normal. The final 
status for the verification of the pull request will therefore depend on the 
exit status of the *jobs* that are run as normal - if they return `pass` then
the entire build will `pass`, and similarly for fail. 

For example, if files inside `aspnet/` are modified as part of a pull request, 
then every *job* but `aspnet` is guaranteed to return `pass`. The return code 
of the `aspnet` job will then determine the final exit status of the build. 

#### Running Travis in a Fork

A Travis account specific to your fork of TFB is highly valuable, as you have 
personal limits on *workers* and can therefore see results from Travis much 
more quickly than you could when the Travis account for TechEmpower has a 
full queue awaiting verification. 

You will need to modify the `.travis.yml` file to contain your own (encrypted)
`GH_TOKEN` environment variable. Unfortunately there is no way to externalize 
encrypted variables, and therefore you will have to manually ensure that you 
don't include your changes to `.travis.yml` in any pull request or commit to 
master!
