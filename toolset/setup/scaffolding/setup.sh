#!/bin/bash

# This file describes how to gather the prerequisites of your test implementation, set
# them up, and finally execute your test application(s) and exit in a state where the
# test is read to respond to HTTP requests to the port/urls described in your
# benchmark_config.json


# fw_depends will search toolset/setup/linux/** for named shell files and execute them.
# These files will set up the sandboxed runtime to have the softwares required when your
# test is ready to go. For example:
#
#   fw_depends Java
# 
# If you are adding a new piece of software, ensure that you first create the setup 
# script in the appropriate place and that it follows the same paradigms illustrated
# in the existing scripts.


# Very often, you will need variables to be set up in order to run your application
# implementation. The suite provides several variables to this shell script to be used
# for just this reason.
#
#   $DBHOST = the IP address of the database machine
#   $TROOT  = the test's root directory (the directory in which this file resides)
#   $FWROOT = the framework benchmark root (the root of this repository)
#   $IROOT  = the sandbox installation root directory (your installed software as well
#             as anything installed via fw_depends is inside this dir)
#   $MAX_CONCURRENCY
#           = the concurrently levels set from the suite configuration file
#
# Below is an example of how to replace a connect string in an application config file
# so that the application will start up with the correct IP:
#
#   sed -i 's|db.ConnectString = .*/|db.ConnectString = '"$DBHOST"':3306/|g' app.conf


# Lastly, you will need to start your test implementation application in a daemon or
# detached mode. For example:
#
#   go run hello.go &


# Note: all comments except for the first line of this file can be deleted.