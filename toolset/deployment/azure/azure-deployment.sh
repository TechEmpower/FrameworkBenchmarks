#!/bin/bash  
#
# Bash script to deploy Web Framework Benchmarks on Windows Azure.
#
# Note: These scripts are designed to run both on Windows (under Cygwin) and
# on Linux and Mac. To achieve that, they have to use some workarounds that
# wouldn't seem necessary or usual in a pure Linux environment.
#
set -o nounset -o errexit

BENCHMARK_DEPLOYMENT_HOME="toolset/deployment"
if [ ! -d "$BENCHMARK_DEPLOYMENT_HOME" ]; then echo "Could not find the '$BENCHMARK_DEPLOYMENT_HOME' directory. This script must be run from the FrameworkBenchmarks directory."; exit 1; fi
source "$BENCHMARK_DEPLOYMENT_HOME/common/bash-common.sh"
source "$BENCHMARK_DEPLOYMENT_HOME/azure/azure-deployment-configuration.sh"

function azure_check_configuration {
    set +o nounset
    
    # Validate AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION.
    if [ -z "$AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION" ]; then fail "AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION is not defined."; fi
    if [ ! -f "$AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION" ]; then fail "File not found: '$AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION'. Use the absolute pathname without shortcuts like '~'."; fi

    # Validate AZURE_DEPLOYMENT_SUBSCRIPTION.
    if [ -z "$AZURE_DEPLOYMENT_SUBSCRIPTION" ]; then fail "AZURE_DEPLOYMENT_SUBSCRIPTION is not defined."; fi

    # Validate AZURE_DEPLOYMENT_LOCATION.
    if [ -z "$AZURE_DEPLOYMENT_LOCATION" ]; then fail "AZURE_DEPLOYMENT_LOCATION is not defined."; fi

    # Validate AZURE_DEPLOYMENT_NAME.
    if [ -z "$AZURE_DEPLOYMENT_NAME" ]; then fail "AZURE_DEPLOYMENT_NAME is not defined."; fi
    if [ ${#AZURE_DEPLOYMENT_NAME} -gt 12 ]; then fail "AZURE_DEPLOYMENT_NAME must be at most 12 characters long."; fi

    # Validate $AZURE_WINDOWS_PASSWORD.
    if [ -z "$AZURE_WINDOWS_PASSWORD" ]; then fail "AZURE_WINDOWS_PASSWORD is not defined."; fi
    if [ ${#AZURE_WINDOWS_PASSWORD} -lt 10 ]; then fail "AZURE_WINDOWS_PASSWORD must be at least 10 characters long."; fi

    # Validate AZURE_DEPLOYMENT_VM_SIZE.
    if [ -z "$AZURE_DEPLOYMENT_VM_SIZE" ]; then fail "AZURE_DEPLOYMENT_VM_SIZE is not defined."; fi
    
    set -o nounset
}

function azure_set_variables {
    # Set variables used in several steps.
    AZURE_LINUX_USER="ubuntu"
    AZURE_SSH_DIR="$HOME/.ssh"
    AZURE_KEY_NAME="id_rsa-${AZURE_DEPLOYMENT_NAME}"
    AZURE_KEY_FILE="${AZURE_SSH_DIR}/${AZURE_KEY_NAME}"
    AZURE_PEM_FILE="${AZURE_SSH_DIR}/${AZURE_KEY_NAME}.x509.pub.pem"
    AZURE_CER_FILE="${AZURE_SSH_DIR}/${AZURE_KEY_NAME}.cer"
    CLIENT_VM_NAME="${AZURE_DEPLOYMENT_NAME}cli"
    WINDOWS_VM_ADMIN="FBMAdmin"
    LINUX_SERVER_VM_NAME="${AZURE_DEPLOYMENT_NAME}lsr"
    WINDOWS_SERVER_VM_NAME="${AZURE_DEPLOYMENT_NAME}wsr"
    SQL_SERVER_VM_NAME="${AZURE_DEPLOYMENT_NAME}sql"
    BENCHMARK_WORKING_DIR="$HOME/tmp/${AZURE_DEPLOYMENT_NAME}"
    mkdir -p "$BENCHMARK_WORKING_DIR" || fail "Could not create directory $BENCHMARK_WORKING_DIR."
    AZURE_DEPLOYMENT_INSTRUCTIONS_FILE="$BENCHMARK_WORKING_DIR/azure-instructions.txt"
    AZURE_CONFIGURATION_OUTPUT_FILE="$BENCHMARK_WORKING_DIR/deployment-configuration.sh"
    BENCHMARK_REPOSITORY=${BENCHMARK_REPOSITORY:-"https://github.com/TechEmpower/FrameworkBenchmarks.git"}
    BENCHMARK_BRANCH=${BENCHMARK_BRANCH:-"master"}

    # Under CYGWIN this script uses a helper function to call the Windows Azure command line program.
    if iscygwin; then
        AZURE_COMMAND="cywgin_cmd azure.cmd"
    else
        AZURE_COMMAND="azure"
    fi
    # TODO azure.cmd is not returning status code from node
}

function azure_configure_command_line_tools {
    information "******************************************************************************"
    information "Windows Azure Deployment: Configuring command line tools"
    information "******************************************************************************"

    # Create directory for Windows Azure command line tools configuration.
    AZURE_HOME="$HOME/.azure"
    echo ""
    echo "Creating Windows Azure configuration directory at $AZURE_HOME"
    mkdir -p ${AZURE_HOME} || fail "Error creating directory $AZURE_HOME."

    # Import publish settings.
    echo ""
    echo "Importing publish settings at $AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION"
    $AZURE_COMMAND account import $AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION || fail "Error importing publish settings."

    # Set default subscription.
    echo ""
    echo "Setting default subscription to $AZURE_DEPLOYMENT_SUBSCRIPTION"
    $AZURE_COMMAND account set "$AZURE_DEPLOYMENT_SUBSCRIPTION" || fail "Error setting default subscription."

    echo ""
}

function azure_create_common_resources {
    information "******************************************************************************"
    information "Windows Azure Deployment: Creating common resources"
    information "******************************************************************************"

    # Create affinity group.
    echo ""
    echo "Creating affinity group $AZURE_DEPLOYMENT_NAME at $AZURE_DEPLOYMENT_LOCATION"
    $AZURE_COMMAND account affinity-group create "$AZURE_DEPLOYMENT_NAME" --location "$AZURE_DEPLOYMENT_LOCATION" --label "$AZURE_DEPLOYMENT_NAME" || fail "Error creating affinity group $AZURE_DEPLOYMENT_NAME."

    # Create storage account.
    echo ""
    echo "Creating storage account $AZURE_DEPLOYMENT_NAME"
    $AZURE_COMMAND account storage create $AZURE_DEPLOYMENT_NAME --affinity-group $AZURE_DEPLOYMENT_NAME || fail "Error creating storage account $AZURE_DEPLOYMENT_NAME."

    # Create virtual network.
    echo ""
    echo "Creating virtual network $AZURE_DEPLOYMENT_NAME"
    $AZURE_COMMAND network vnet create $AZURE_DEPLOYMENT_NAME --affinity-group $AZURE_DEPLOYMENT_NAME || fail "Error creating virtual network $AZURE_DEPLOYMENT_NAME."

    # Create directory for keys.
    echo ""
    echo "Creating directory for keys at $AZURE_SSH_DIR"
    mkdir -p ${AZURE_SSH_DIR} || fail "Error creating directory $AZURE_SSH_DIR."

    # Create key files.
    echo "Creating key pair at $AZURE_KEY_FILE"
    ssh-keygen -t rsa -b 2048 -f "$AZURE_KEY_FILE" -C "$AZURE_KEY_NAME" -q -N "" || fail "Error creating SSH key."
    chmod 600 "$AZURE_KEY_FILE"
    warning "-----------------------------------------------------------------------"
    warning "Protect this key file. It has no passphrase and is stored in plaintext."
    warning "-----------------------------------------------------------------------"

    echo "Creating PEM file at $AZURE_PEM_FILE"
    openssl req -new -x509 -days 365 -subj "/CN=$AZURE_DEPLOYMENT_NAME/O=Web Framework Benchmarks" -key "$AZURE_KEY_FILE" -out "$AZURE_PEM_FILE" || fail "Error creating PEM file."
    chmod 600 "$AZURE_PEM_FILE"

    echo "Creating CER file at $AZURE_CER_FILE"
    openssl x509 -outform der -in "$AZURE_PEM_FILE" -out "$AZURE_CER_FILE" || fail "Error creating CER file."
    chmod 600 "$AZURE_CER_FILE"

    echo ""
}

function azure_create_vms {
    information "******************************************************************************"
    information "Windows Azure Deployment: Creating virtual machines"
    information "******************************************************************************"

    # Get latest Ubuntu Server 12.04 daily VM image.
    echo ""
    echo "Latest Ubuntu Server 12.04 image:"
    LATEST_UBUNTU_IMAGE=$($AZURE_COMMAND vm image list | grep Ubuntu_DAILY_BUILD-precise-12_04_4-LTS-amd64-server | sort | tail -1 | cut -c 10-120)
    if [ -z "$LATEST_UBUNTU_IMAGE" ]; then fail "Unable to find Ubuntu_DAILY_BUILD-precise-12_04 in Azure vm image list."; fi
    echo "Found ubuntu image $LATEST_UBUNTU_IMAGE"

    # Create client VM.
    echo ""
    echo "Creating client VM: $CLIENT_VM_NAME"
    $AZURE_COMMAND vm create $CLIENT_VM_NAME $LATEST_UBUNTU_IMAGE $AZURE_LINUX_USER --ssh-cert "$AZURE_PEM_FILE" --no-ssh-password --vm-name $CLIENT_VM_NAME --vm-size $AZURE_DEPLOYMENT_VM_SIZE --virtual-network-name $AZURE_DEPLOYMENT_NAME --ssh --affinity-group $AZURE_DEPLOYMENT_NAME || fail "Error creating virtual machine $CLIENT_VM_NAME."

    # Create Ubuntu server VM.
    echo ""
    echo "Creating Linux server VM: $LINUX_SERVER_VM_NAME"
    $AZURE_COMMAND vm create $LINUX_SERVER_VM_NAME $LATEST_UBUNTU_IMAGE $AZURE_LINUX_USER --ssh-cert "$AZURE_PEM_FILE" --no-ssh-password --vm-name $LINUX_SERVER_VM_NAME --vm-size $AZURE_DEPLOYMENT_VM_SIZE --virtual-network-name $AZURE_DEPLOYMENT_NAME --ssh --affinity-group $AZURE_DEPLOYMENT_NAME || fail "Error creating virtual machine $LINUX_SERVER_VM_NAME."

    # Get latest Windows Server 2012 Datacenter image.
    echo ""
    echo "Latest Windows Server 2012 Datacenter image:"
    LATEST_WINDOWS_IMAGE=$($AZURE_COMMAND vm image list | grep Windows-Server-2012-Datacenter | sort | tail -1 | cut -c 10-120)
    echo $LATEST_WINDOWS_IMAGE

    # Create Windows server VM.
    echo ""
    echo "Creating Windows server VM: $WINDOWS_SERVER_VM_NAME"
    $AZURE_COMMAND vm create $WINDOWS_SERVER_VM_NAME $LATEST_WINDOWS_IMAGE $WINDOWS_VM_ADMIN $AZURE_WINDOWS_PASSWORD --vm-name $WINDOWS_SERVER_VM_NAME --vm-size $AZURE_DEPLOYMENT_VM_SIZE --virtual-network-name $AZURE_DEPLOYMENT_NAME --rdp --affinity-group $AZURE_DEPLOYMENT_NAME || fail "Error creating virtual machine $WINDOWS_SERVER_VM_NAME."

    # Create SQL Server VM.
    echo ""
    echo "SQL Server image:"
    SQL_SERVER_IMAGE="fb83b3509582419d99629ce476bcb5c8__Microsoft-SQL-Server-2012SP1-CU4-11.0.3368.0-Standard-ENU-Win2012"
    echo $SQL_SERVER_IMAGE
    echo ""
    echo "Creating SQL Server VM: $SQL_SERVER_VM_NAME"
    $AZURE_COMMAND vm create $SQL_SERVER_VM_NAME $SQL_SERVER_IMAGE $WINDOWS_VM_ADMIN $AZURE_WINDOWS_PASSWORD --vm-name $SQL_SERVER_VM_NAME --vm-size $AZURE_DEPLOYMENT_VM_SIZE --virtual-network-name $AZURE_DEPLOYMENT_NAME --rdp --affinity-group $AZURE_DEPLOYMENT_NAME || fail "Error creating virtual machine $SQL_SERVER_VM_NAME."

    echo ""
}

function azure_get_vm_ip {
    local vm_name=$1
    local vm_status=""
    until [[ "$vm_status" == "ReadyRole" ]]
    do
        local vm_properties=`$AZURE_COMMAND vm show $vm_name --json`
        json=$vm_properties
        prop='InstanceStatus'
        local vm_status=`jsonval`
        if [[ "$vm_status" != "ReadyRole" ]]; then { error "$vm_name status is '$vm_status'. Retrying in 30s..."; sleep 30s; } fi
    done
    prop='IPAddress'
    local ip_address=`jsonval`
    if [ -z "$ip_address" ]; then fail "ip_address not found."; fi
    echo $ip_address
}

function azure_save_deployment_configuration {
    information "******************************************************************************"
    information "Windows Azure Deployment: Creating deployment configuration file"
    information "******************************************************************************"
    
    # Get client IP.
    echo "Retrieving $CLIENT_VM_NAME IP:"
    CLIENT_IP=`azure_get_vm_ip $CLIENT_VM_NAME`
    echo "$CLIENT_IP"    
    
    # Get Linux server IP.
    echo ""
    echo "Retrieving $LINUX_SERVER_VM_NAME IP:"
    LINUX_SERVER_IP=`azure_get_vm_ip $LINUX_SERVER_VM_NAME`
    echo "$LINUX_SERVER_IP"

    echo ""
    echo "Saving configuration file at $AZURE_CONFIGURATION_OUTPUT_FILE"

    cat >$AZURE_CONFIGURATION_OUTPUT_FILE <<_EOF_
BENCHMARK_LINUX_CLIENT="$CLIENT_VM_NAME.cloudapp.net"
BENCHMARK_LINUX_CLIENT_IP="$CLIENT_IP"
BENCHMARK_LINUX_SERVER="$LINUX_SERVER_VM_NAME.cloudapp.net"
BENCHMARK_LINUX_SERVER_IP="$LINUX_SERVER_IP"
BENCHMARK_LINUX_USER="$AZURE_LINUX_USER"
BENCHMARK_SSH_KEY="$AZURE_KEY_FILE"
BENCHMARK_WINDOWS_SERVER="$WINDOWS_SERVER_VM_NAME.cloudapp.net"
BENCHMARK_WINDOWS_SERVER_USER="$WINDOWS_SERVER_VM_NAME\\$WINDOWS_VM_ADMIN"
BENCHMARK_SQL_SERVER="$SQL_SERVER_VM_NAME.cloudapp.net"
BENCHMARK_SQL_SERVER_USER="$SQL_SERVER_VM_NAME\\$WINDOWS_VM_ADMIN"
BENCHMARK_WORKING_DIR="$BENCHMARK_WORKING_DIR"
BENCHMARK_REPOSITORY="$BENCHMARK_REPOSITORY"
BENCHMARK_BRANCH="$BENCHMARK_BRANCH"
_EOF_

    echo ""
}

function azure_print_instructions {
    information "******************************************************************************"
    information "Windows Azure Deployment: Instructions"
    information "******************************************************************************"

    AZURE_VMS_DEPLOYED=`$AZURE_COMMAND vm list | grep -E "DNS Name|$AZURE_DEPLOYMENT_NAME" | cut -c 10-`

    if iscygwin; then
        AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION=`cygpath -w "$AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION"`
    fi

    cat >$AZURE_DEPLOYMENT_INSTRUCTIONS_FILE <<_EOF_
Windows Azure resources provisioned at $AZURE_DEPLOYMENT_LOCATION with the base name $AZURE_DEPLOYMENT_NAME under the subscription $AZURE_DEPLOYMENT_SUBSCRIPTION using $AZURE_DEPLOYMENT_VM_SIZE virtual machines.

VMs deployed:
$AZURE_VMS_DEPLOYED

To connect to the client VM:
ssh $AZURE_LINUX_USER@$CLIENT_VM_NAME.cloudapp.net -i $AZURE_KEY_FILE

To connect to the Linux server VM:
ssh $AZURE_LINUX_USER@$LINUX_SERVER_VM_NAME.cloudapp.net -i $AZURE_KEY_FILE

To connect to the Windows server VM:
mstsc /v:$WINDOWS_SERVER_VM_NAME.cloudapp.net /admin /f
User name: $WINDOWS_SERVER_VM_NAME\Administrator

To connect to the SQL Server VM:
mstsc /v:$SQL_SERVER_VM_NAME.cloudapp.net /admin /f
User name: $SQL_SERVER_VM_NAME\Administrator

To manage the Windows Azure resources:
https://manage.windowsazure.com

For your security delete the publish settings file when you don't need it anymore:
$AZURE_DEPLOYMENT_PUBLISHSETTINGS_LOCATION

Remember to stop the virtual machines when they're not needed anymore.
VMs in "Stopped (Deallocated)" status don't incur in computing costs.
Virtual disks (VHD) incur in storage costs until they are deleted.

Deployment configuration file saved at:
$AZURE_CONFIGURATION_OUTPUT_FILE
_EOF_

    # TODO: instructions to stop and start the VMs
    # TODO: instructions to list tests available at each server
    # TODO: instructions to run tests

    echo ""
    cat $AZURE_DEPLOYMENT_INSTRUCTIONS_FILE
    
    if iscygwin; then
        AZURE_DEPLOYMENT_INSTRUCTIONS_FILE=`cygpath -w "$AZURE_DEPLOYMENT_INSTRUCTIONS_FILE"`
    fi

    echo ""
    warning "You can review these instructions at $AZURE_DEPLOYMENT_INSTRUCTIONS_FILE"
    
    echo ""
}

function azure_continue_deployment {
    echo "Starting automated deployment on the provisioned VMs..."
    echo ""
    run_bash "$BENCHMARK_DEPLOYMENT_HOME/common/deployment.sh" "$AZURE_CONFIGURATION_OUTPUT_FILE"
}

information "Deploying Web Framework Benchmarks to Windows Azure..."
echo ""

azure_check_configuration
azure_set_variables

echo "The resources will be created with the base name $AZURE_DEPLOYMENT_NAME" \
"in $AZURE_DEPLOYMENT_LOCATION under the subscription $AZURE_DEPLOYMENT_SUBSCRIPTION" \
"using $AZURE_DEPLOYMENT_VM_SIZE virtual machines."
echo "The benchmark suite will be cloned from the $BENCHMARK_BRANCH branch of the repository at $BENCHMARK_REPOSITORY."
echo ""

azure_configure_command_line_tools
azure_create_common_resources
azure_create_vms
azure_save_deployment_configuration
azure_print_instructions
azure_continue_deployment

warning "Instructions were saved to $AZURE_DEPLOYMENT_INSTRUCTIONS_FILE"
echo ""
