#!/bin/bash  
#
# This script installs prerequisites on the Linux server, clones the benchmark repository
# and performs initial Linux server setup.
#
# It should be executed on a host provisioned according to the deployment documentation,
# with the prescribed operating system version and no additional software.
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
information "Benchmark Suite Deployment: Linux server setup"
information "******************************************************************************"

BENCHMARK_REMOTE_CONFIGURATION_FILE="$BENCHMARK_WORKING_DIR/benchmark-configuration.sh"
BENCHMARK_REMOTE_KEY_FILE="~/.ssh/benchmark-key"

# Create Linux host configuration script.
echo ""
echo "Creating Linux host configuration script at $BENCHMARK_REMOTE_CONFIGURATION_FILE"
cat >$BENCHMARK_REMOTE_CONFIGURATION_FILE <<_EOF_
#!/bin/bash
export BENCHMARK_HOME=~/FrameworkBenchmarks
export BENCHMARK_SERVER_IP=$BENCHMARK_LINUX_SERVER_IP
export BENCHMARK_CLIENT_IP=$BENCHMARK_LINUX_CLIENT_IP
export BENCHMARK_KEY_PATH=$BENCHMARK_REMOTE_KEY_FILE
export BENCHMARK_REPOSITORY=$BENCHMARK_REPOSITORY
export BENCHMARK_BRANCH=$BENCHMARK_BRANCH
_EOF_

# Upload Linux host configuration script.
echo ""
upload_file "$BENCHMARK_REMOTE_CONFIGURATION_FILE" "$BENCHMARK_LINUX_USER" "$BENCHMARK_LINUX_SERVER" "~" "$BENCHMARK_SSH_KEY"

# Copy key to server.
echo ""
upload_file "$BENCHMARK_SSH_KEY" "$BENCHMARK_LINUX_USER" "$BENCHMARK_LINUX_SERVER" "$BENCHMARK_REMOTE_KEY_FILE" "$BENCHMARK_SSH_KEY"

# Install software.
echo ""
run_remote_script "Installing benchmark software on the Linux server. This may take about 2 hours." "$BENCHMARK_LINUX_USER" "$BENCHMARK_LINUX_SERVER" "$BENCHMARK_SSH_KEY" "$BENCHMARK_DEPLOYMENT_HOME/common/remote/lsr-step-1.sh" "$BENCHMARK_WORKING_DIR/lsr-step-1.log" || fail "Error running script."

echo ""
