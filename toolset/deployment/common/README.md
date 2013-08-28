# Benchmark Suite Automated Deployment

This directory contains tools and documentation to automatically deploy the benchmark suite on an environment already provisioned according to the instructions in the [Benchmark Suite Deployment README file](../README.md).

You can use this as an easier alternative to the manual setup process which is documented in the [Benchmark Suite Setup README file](../../setup/README.md).

Notice that this script will be automatically executed by other scripts, such as the [Windows Azure deployment script](../azure/README.md). In this case, you don't need to configure or execute this script yourself.


## Prerequisites

The tooling is cross-platform, based on bash scripts and Unix tools.

* On Windows, run the PowerShell script `InstallCygwin.ps1` (in the toolset/deployment/common directory) to install Cygwin.
* On Linux and OS X, these tools are natively available.


## Configuration

The deployment parameters are provided through a configuration file that you can create in a temporary directory.

This is an example file:

    $ cat ~/tmp/deployment-configuration.sh
    BENCHMARK_LINUX_CLIENT=wfb08291038cli.cloudapp.net
    BENCHMARK_LINUX_CLIENT_IP=10.32.0.4
    BENCHMARK_LINUX_SERVER=wfb08291038lsr.cloudapp.net
    BENCHMARK_LINUX_SERVER_IP=10.32.0.12
    BENCHMARK_LINUX_USER=ubuntu
    BENCHMARK_SSH_KEY=/home/user/.ssh/id_rsa-wfb08291038
    BENCHMARK_WINDOWS_SERVER=wfb08291038wsr.cloudapp.net
    BENCHMARK_WINDOWS_SERVER_USER=wfb08291038wsr\Administrator
    BENCHMARK_SQL_SERVER=wfb08291038sql.cloudapp.net
    BENCHMARK_SQL_SERVER_USER=wfb08291038sql\Administrator
    BENCHMARK_WORKING_DIR=/home/user/tmp/wfb08291038
    BENCHMARK_REPOSITORY=https://github.com/TechEmpower/FrameworkBenchmarks.git
    BENCHMARK_BRANCH=master

The parameters are:

* **BENCHMARK_LINUX_CLIENT**: The DNS name or IP address to be used to connect to the Linux client host for administrative purposes. Usually this will be an address on the public Internet.
* **BENCHMARK_LINUX_CLIENT_IP**: The IP address that the other benchmark hosts should use to connect to the Linux client host. Usually this will be a private (internal) network address.
* **BENCHMARK_LINUX_SERVER**: The DNS name or IP address to be used to connect to the Linux server host for administrative purposes. Usually this will be an address on the public Internet.
* **BENCHMARK_LINUX_SERVER_IP**: The IP address that the other benchmark hosts should use to connect to the Linux server host. Usually this will be a private (internal) network address.
* **BENCHMARK_LINUX_USER**: The name of the administrative user on both the Linux client and the Linux server.
* **BENCHMARK_SSH_KEY**: The private key of the administrative user on both the Linux client and the Linux server.
* **BENCHMARK_WINDOWS_SERVER**: The DNS name or IP address to be used to connect to the Windows Server host for administrative purposes. Usually this will be an address on the public Internet.
* **BENCHMARK_WINDOWS_SERVER_USER**: The fully qualified name of the administrative user on the Windows Server host.
* **BENCHMARK_SQL_SERVER**: The DNS name or IP address to be used to connect to the SQL Server host for administrative purposes. Usually this will be an address on the public Internet.
* **BENCHMARK_SQL_SERVER_USER**: The fully qualified name of the administrative user on the SQL Server host.
* **BENCHMARK_WORKING_DIR**: An existing directory where the deployment scripts can save temporary and log files.
* **BENCHMARK_REPOSITORY**: The Git repository of the benchmark suite. Use a custom repository address if you want to deploy from a fork. Otherwise use the official one (https://github.com/TechEmpower/FrameworkBenchmarks.git).
* **BENCHMARK_BRANCH**: The Git branch of the benchmark suite. Use a custom branch name if you want to deploy from another branch. Otherwise use the official one (master).


## Deployment

On Linux or OS X, considering the framework was cloned at "~/", execute these commands in a shell:

    cd ~/FrameworkBenchmarks
    nohup toolset/deployment/common/deployment.sh &> deployment.log &
    tail -f deployment.log

On Windows, first install Cygwin running `InstallCygwin.ps1` then, considering the framework was cloned at "C:\", execute these commands in a command prompt:

    C:
    cd \FrameworkBenchmarks
    bash -o igncr toolset/deployment/common/deployment.sh

Notice that the deployment process will take several hours. During this time the machine that is running it will have to be online, otherwise the deployment process will be interrupted and you'll probably have to start over from clean hosts.


## Diagnostics

The deployment script will output progress messages. You can watch these messages in the terminal, or redirect them to an output file (such as on the Linux example above) and watch this file.

The deployment process is composed of several steps. For each step, one or more log files may be created. Their pathname will be shown and you can open another terminal to watch the progress of that step, like this:

    tail -f /home/user/tmp/wfb08291038/lsr-step-1.log

The first step (lsr-step-1, for "Linux server step 1") will install the dependencies on the Linux server and, from it, on the Linux client. This is a long process and you can see an overview of its progress with a command like this:

    grep '^INSTALL' /home/user/tmp/wfb08291038/lsr-step-1.log
    
Or, for watching continuously:

    tail -f /home/user/tmp/wfb08291038/lsr-step-1.log | grep '^INSTALL' 

You can review this step for installation errors with this command:

    grep '^INSTALL ERROR' /home/user/tmp/wfb08291038/lsr-step-1.log

In case one step fails, you'll usually find the error message at the end of the log, and you can review it like this:

    tail deployment.log
    tail /home/user/tmp/wfb08291038/lsr-step-2.log

If there are errors, the safest option would be to start the deployment process all over, provisioning new and clean hosts. The benchmark suite and its tests have a great number of dependencies that have to be installed in a particular order. If one of these steps fail, it may compromise the entire suite or an entire class of tests.

With that said, depending on the kind of error, you could either ignore it (for instance if it compromises a test you don't need to run) or attempt to solve it.

Another option would be to perform the setup manually as described in the [Benchmark Suite Setup README file](../../setup/README.md).


## Support
* [Google Groups](https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks) for support specific to the Web Framework Benchmarks project.
* [Server Fault](http://serverfault.com/) for questions related to server administration.
* [Ask Ubuntu](http://askubuntu.com/) for questions related to Ubuntu.

If you find that the automated deployment process is not working as it should please [open an issue](https://github.com/TechEmpower/FrameworkBenchmarks/issues/new), attaching a link to all relevant log files.
