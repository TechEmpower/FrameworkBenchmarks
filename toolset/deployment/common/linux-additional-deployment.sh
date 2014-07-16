#!/bin/bash  
#
# This scripts performs additional setup on the Linux server and the Linux client.
#
# It should be executed after 'linux-initial-deployment.sh'.
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
information "Benchmark Suite Deployment: Linux server and client additional setup"
information "******************************************************************************"

# Reboot Linux client.
echo ""
reboot_linux_host "$BENCHMARK_LINUX_USER" "$BENCHMARK_LINUX_CLIENT" "$BENCHMARK_SSH_KEY" || fail "Error rebooting $BENCHMARK_LINUX_CLIENT."

# Reboot Linux server.
echo ""
reboot_linux_host "$BENCHMARK_LINUX_USER" "$BENCHMARK_LINUX_SERVER" "$BENCHMARK_SSH_KEY" || fail "Error rebooting $BENCHMARK_LINUX_SERVER."

# Additional setup.
echo ""
run_remote_script "Completing setup." "$BENCHMARK_LINUX_USER" "$BENCHMARK_LINUX_SERVER" "$BENCHMARK_SSH_KEY" "$BENCHMARK_DEPLOYMENT_HOME/common/remote/lsr-step-2.sh" "$BENCHMARK_WORKING_DIR/lsr-step-2.log" || fail "Error running script."

# Verify setup
echo ""
run_remote_script "Verifying setup." "$BENCHMARK_LINUX_USER" "$BENCHMARK_LINUX_SERVER" "$BENCHMARK_SSH_KEY" "$BENCHMARK_DEPLOYMENT_HOME/common/remote/lsr-step-3.sh" "$BENCHMARK_WORKING_DIR/lsr-step-3.log" || fail "Error running script."

echo ""
