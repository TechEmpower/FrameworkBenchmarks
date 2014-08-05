# TFB Toolset 

This directory contains the code that TFB uses to automate installing, 
launching, load testing, and terminating each framework. 

## Travis Integration

This section details how TFB integrates with travis-ci.org. At a 
high level, there is a github hook that notifies travis-ci every 
time new commits are pushed to master, or every time new commits 
are pushed to a pull request. This causes travis to spin up a 
virtual machine, checkout the code, and run an installation and 
a verification. 

### Terminology

Each push to github triggers a new travis *build*. Each *build* 
contains a number of independent *jobs*. Each *job* is run on an
isolated virtual machine called a *worker*. 

Our project has one *job* for each framework directory, e.g. 
one *job* for `go`, one *job* for `activeweb`, etc. Each 
*job* gets it's own *worker* virtual machine that runs the 
installation for that framework (using `--install server`) and 
verifies the framework's output using `--mode verify`. 

### Travis Limits

Travis is a free (pro available) service, and therefore imposes 
multiple limits. 

Each time someone pushes new commits to master (or to a pull request), 
a new *build* is triggered that contains ~100 *jobs*, one for each 
framework directory. This obviously is resource intensive, so it is 
critical to understand travis limits. 

**Minutes Per Job**: 50 minutes maxiumum. None of the *job*s we run hit 
this limit (most take 10-15 minutes total)

**Max Concurrent Jobs**: Typically 4, but based on Travis' load. This is 
our main limiting factor, as each *build* causes ~100 *jobs*. This is 
discussed below. 

**Max Console Output**: A *job* can only ouput `4MB` of log data before it 
is terminated by Travis. Some of our larger builds (e.g. `aspnet`) run into 
this limit, but most do not

### Dealing with Max Concurrent Jobs

### .travis.yml File

### Run-Continuous Integration (e.g. run-ci.py) Script
