#!/bin/bash  
#
# Bash script to deploy Web Framework Benchmarks on Windows Azure.
#
# Step 7: SQL Server setup.
#
# For now, SQL Server setup is done manually.
#
set -o nounset -o errexit

BENCHMARK_DEPLOYMENT_HOME="toolset/deployment"
if [ ! -d "$BENCHMARK_DEPLOYMENT_HOME" ]; then echo "Could not find the '$BENCHMARK_DEPLOYMENT_HOME' directory. This script must be run from the FrameworkBenchmarks directory."; exit 1; fi
source "$BENCHMARK_DEPLOYMENT_HOME/common/bash-common.sh"

if [ -z "$1" ]; then fail "The deployment configuration file name must be provided as an argument."; fi
BENCHMARK_DEPLOYMENT_CONFIGURATION=$1
if [ ! -f $BENCHMARK_DEPLOYMENT_CONFIGURATION ]; then fail "File not found: $BENCHMARK_DEPLOYMENT_CONFIGURATION"; fi
source "$BENCHMARK_DEPLOYMENT_CONFIGURATION"

information "******************************************************************************"
information "Benchmark Suite Deployment: SQL Server setup"
information "******************************************************************************"

LOCAL_SQLSERVER_BOOTSTRAP_FILE="toolset/setup/windows/setup-sqlserver-bootstrap.ps1"
if iscygwin; then
    LOCAL_SQLSERVER_BOOTSTRAP_FILE=`cygpath -w "$LOCAL_SQLSERVER_BOOTSTRAP_FILE"`
fi

cat <<_EOF_

Instructions for SQL Server setup:

1. Connect to the SQL Server using this command:
mstsc /v:$BENCHMARK_SQL_SERVER /admin /f
User name is $BENCHMARK_SQL_SERVER_USER and the password is the one you defined earlier.
2. Copy to the remote server the file located at
$LOCAL_SQLSERVER_BOOTSTRAP_FILE
3. On the remote server, right-click on the pasted file and select "Run with PowerShell".

_EOF_
