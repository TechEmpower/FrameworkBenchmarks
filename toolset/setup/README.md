# Benchmark Suite Setup

This directory contains scripts to install the benchmark suite on adequately provisioned hosts.

It is composed of these directories:

* `linux`: Scripts to setup the Linux server and the Linux client and database server.
* `sqlserver`: Scripts to setup the SQL Server database server.
* `windows`: Scripts to setup the Windows server.

The setup process is documented below.

Notice that, as an alternative to the manual setup process, you can use the [automated deployment script](../deployment/common/README.md).

If these instructions get out of date or require clarification, please [open an issue](https://github.com/TechEmpower/FrameworkBenchmarks/issues/new).


## Prerequisites

Before starting setup, all the required hosts must be provisioned, with the respective operating system and required software installed, and with connectivity for remote management (SSH on Linux, RDP and WinRM on Windows).

Refer to [Benchmark Suite Deployment README file](../deployment/README.md) for the provisioning procedures documentation.

## Linux server and client setup

To coordinate the tests via scripting, the servers need to be able to work together. So once the instances are running, the first thing you'll want to do is copy your ssh key to the application server instance so that you can ssh between the two machines:

	sftp -i path-to-ssh-key ubuntu@server-instance-ip
	put path-to-ssh-key .ssh/
	exit

This will copy your private ssh key file to ~/.ssh on the Linux server, where it will be used to connect to the benchmark client.

Now, connect via ssh to the Linux server:

    ssh -i path-to-ssh-key ubuntu@server-instance-ip

On the Linux server, check out the latest version from GitHub:

	cd ~
    sudo apt-get install git -qq
	git clone https://github.com/TechEmpower/FrameworkBenchmarks.git
	cd FrameworkBenchmarks

As it currently stands, the script that runs the tests makes some assumptions about where the code is placed. We assume that the FrameworkBenchmarks repository will be located in your home directory.

Next, we're going to setup the Linux server and the client with the required software:

	toolset/run-tests.py -s server-private-ip -c client-private-ip -i path-to-ssh-key --install-software --list-tests &> install.log
    
This step can take a couple hours. If you like, you can open another shell and watch the progress:

    tail -f ~/FrameworkBenchmarks/install.log | grep '^INSTALL'

After the installation is done, reboot both the Linux server and the Linux client.

When they are online again, connect via ssh to the Linux server and type these commands for additional setup:

    source ~/.bash_profile
    # For your first time through the tests, set the ulimit for open files
    ulimit -n 8192
    # Most software is installed automatically by the script, but running the mongo command below from
    # the install script was causing some errors. For now this needs to be run manually.
    cd installs/jruby-rack && rvm jruby-1.7.4 do jruby -S bundle exec rake clean gem SKIP_SPECS=true
    cd target && rvm jruby-1.7.4 do gem install jruby-rack-1.2.0.SNAPSHOT.gem
    cd ../../..
    cd installs && curl -sS https://getcomposer.org/installer | php -- --install-dir=bin
    cd ..
    sudo apt-get remove --purge openjdk-6-jre openjdk-6-jre-headless
    mongo --host client-private-ip < config/create.js

You can validate that the setup worked by running a smoke test like this:

    toolset/run-tests.py -s server-private-ip -c client-private-ip -i path-to-ssh-key --max-threads 1 --name smoketest --test servlet-raw --type all -m verify

This should run the verification step for a single framework.

## Windows server setup

* Connect to the Windows server via Remote Desktop.
* Copy `installer-bootstrap.ps1` from "toolset/setup/windows" to the server (use CTRL-C and CTRL-V).
* Copy your Linux client private key too.
* Right click on the installer script and select `Run with PowerShell`.
* Press Enter to confirm.
* It will install git and then launch `installer.ps1` from the repository, which will install everything else.
* The installation takes about 20 minutes.
* Then you have a working console: try `python`, `git`, `ssh`, `curl`, `node` etc. and verify that everything works + PowerShell goodies.

The client/database machine is still assumed to be a Linux box. You can install just the client software via

    python toolset\run-tests.py -s server-private-ip -c client-private-ip -i "C:\Users\Administrator\Desktop\client.key" --install-software --install client --list-tests

but this step is not required if you already installed the Linux server and client as described above.

Now you can run tests:

    python toolset\run-tests.py -s server-private-ip -c client-private-ip -i "C:\Users\Administrator\Desktop\client.key" --max-threads 2 --duration 30 --sleep 5 --name win --test aspnet --type all

## SQL Server setup

* Connect to the SQL Server host via Remote Desktop.
* Run a `Command Prompt` as Administrator.
* Enter this command:

        powershell -ExecutionPolicy Bypass -Command "iex (New-Object Net.WebClient).DownloadString('https://raw.github.com/TechEmpower/FrameworkBenchmarks/master/setup-sqlserver-bootstrap.ps1')"

* This will configure SQL Server, the Windows Firewall, and populate the database.

Now, when running `run-tests.py`, just add `-d <ip of SQL Server instance>`. This works for the (Windows Server-based) `aspnet-sqlserver-raw` and `aspnet-sqlserver-entityframework` tests.
