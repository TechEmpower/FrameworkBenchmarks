#!/bin/bash  
#
# Bash script to deploy Web Framework Benchmarks on hosts that are already provisioned.
#
# This is the master script that executes each step in sequence.
# If a step fails, you can correct the cause and manually execute the missing steps
# without having to restart from scratch.
#
# Note: These scripts are designed to run both on Windows (under Cygwin) and
# on Linux and Mac. To achieve that, they have to use some workarounds that
# wouldn't seem necessary or usual in a pure Linux environment.
#
set -o errexit

BENCHMARK_DEPLOYMENT_HOME="toolset/deployment"
if [ ! -d "$BENCHMARK_DEPLOYMENT_HOME" ]; then echo "Could not find the '$BENCHMARK_DEPLOYMENT_HOME' directory. This script must be run from the FrameworkBenchmarks directory."; exit 1; fi
source "$BENCHMARK_DEPLOYMENT_HOME/common/bash-common.sh"

if [ -z "$1" ]; then fail "The deployment configuration file name must be provided as an argument."; fi
BENCHMARK_DEPLOYMENT_CONFIGURATION=$1
if [ ! -f $BENCHMARK_DEPLOYMENT_CONFIGURATION ]; then fail "File not found: $BENCHMARK_DEPLOYMENT_CONFIGURATION"; fi
source "$BENCHMARK_DEPLOYMENT_CONFIGURATION"

function deployment_check_configuration {
    if [ -z "$BENCHMARK_LINUX_SERVER" ]; then fail "BENCHMARK_LINUX_SERVER is not defined."; fi
    if [ -z "$BENCHMARK_LINUX_SERVER_IP" ]; then fail "BENCHMARK_LINUX_SERVER_IP is not defined."; fi
    if [ -z "$BENCHMARK_LINUX_CLIENT" ]; then fail "BENCHMARK_LINUX_CLIENT is not defined."; fi
    if [ -z "$BENCHMARK_LINUX_CLIENT_IP" ]; then fail "BENCHMARK_LINUX_CLIENT_IP is not defined."; fi
    if [ -z "$BENCHMARK_LINUX_USER" ]; then fail "BENCHMARK_LINUX_USER is not defined."; fi
    if [ -z "$BENCHMARK_LINUX_SERVER" ]; then fail "BENCHMARK_LINUX_SERVER is not defined."; fi
    if [ -z "$BENCHMARK_SSH_KEY" ]; then fail "BENCHMARK_SSH_KEY is not defined."; fi
    if [ ! -f $BENCHMARK_SSH_KEY ]; then fail "File not found: $BENCHMARK_SSH_KEY"; fi
    if [ -z "$BENCHMARK_WORKING_DIR" ]; then fail "BENCHMARK_WORKING_DIR is not defined."; fi
    if [ ! -d "$BENCHMARK_WORKING_DIR" ]; then fail "Directory not found: $BENCHMARK_WORKING_DIR"; fi
    if [ -z "$BENCHMARK_REPOSITORY" ]; then fail "BENCHMARK_REPOSITORY is not defined."; fi
    if [ -z "$BENCHMARK_BRANCH" ]; then fail "BENCHMARK_BRANCH is not defined."; fi
}

information "Deploying Web Framework Benchmarks..."
echo ""

# Execute deployment steps

deployment_check_configuration

run_bash "$BENCHMARK_DEPLOYMENT_HOME/common/linux-initial-deployment.sh" "$BENCHMARK_DEPLOYMENT_CONFIGURATION" || fail "Deployment failed."

run_bash "$BENCHMARK_DEPLOYMENT_HOME/common/linux-additional-deployment.sh" "$BENCHMARK_DEPLOYMENT_CONFIGURATION" || fail "Deployment failed."

if [ ! -z "$BENCHMARK_WINDOWS_SERVER" ]
then
    run_bash "$BENCHMARK_DEPLOYMENT_HOME/common/windows-server-deployment.sh" "$BENCHMARK_DEPLOYMENT_CONFIGURATION" || fail "Deployment failed."
fi

if [ ! -z "$BENCHMARK_SQL_SERVER" ]
then
    run_bash "$BENCHMARK_DEPLOYMENT_HOME/common/sql-server-deployment.sh" "$BENCHMARK_DEPLOYMENT_CONFIGURATION" || fail "Deployment failed."
fi

information "Web Framework Benchmarks deployment finished."
echo ""
