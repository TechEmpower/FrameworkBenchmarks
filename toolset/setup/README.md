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

**NOTE:** If testing a pull request or doing development, it is usually adequate to only use one computer. In that case, your server, client, and database IPs will be 127.0.0.1

### Installing the Framework Benchmark App Server

* Install [Ubuntu 14.04](http://www.ubuntu.com/download/server) with username `tfb`. Ensure that OpenSSH is selected when you install. If not, run the following command
```bash
$ sudo apt-get install openssh-server
```
* If Ubuntu is already installed, run the following command and follow the prompts.
```bash
$ sudo adduser tfb
```
* Log in as `tfb`
* Fully update **NOTE**: If you update the kernel (linux-firmware), it is generally a good idea to reboot aftewards.
```bash
$ sudo apt-get update && sudo apt-get upgrade
```
* Run the command: `sudo visudo`
* Change line 20 in from `%sudo   ALL=(ALL:ALL) ALL` to `%sudo   ALL=NOPASSWD: ALL`
* Run the following **(Don't enter a password, just hit enter when the prompt pops up)**. **NOTE** This is still necessary if the client and database are on the same computer as the server
```bash
$ ssh-keygen
$ ssh-copy-id <database ip>
$ ssh-copy-id <client ip>
```
* Install git and clone the Framework Benchmarks repository
```bash
$ sudo apt-get install git
$ cd ~
$ git clone https://github.com/TechEmpower/FrameworkBenchmarks.git
$ cd FrameworkBenchmarks
```
* Install the server software. This will take a long time
```bash
$ nohup python toolset/run-tests.py -s <server hostname/ip> -c <client hostname/ip> -u tfb --install-software --install server --list-tests &
```
* If you want to view the process of installing, do the following. The session can be interrupted easily so no need to worry about keeping a connection.
```bash
$ tail -f nohup.out
```
* Reboot when the install is done
* Edit your ~/.bashrc file to change the following
 * Change `TFB_SERVER_HOST=<ip address>` to the server's IP address
 * Change `TFB_CLIENT_HOST=<ip address>` to the client's ip address
 * Change `TFB_DATABASE_HOST=<ip address>` to the database's ip address.
 * Change `TFB_CLIENT_IDENTITY_FILE=<path>` to the id file you specified when you ran ssh-keygen (probably /home/tfb/.ssh/id_rsa if you don't know what it is)
 * Run the command `source ~/.bashrc`
* If you are setting up any other servers, do so before proceeding.
* Run the following commands
```bash
cd ~/FrameworkBenchmarks
source ~/.bash_profile
# For your first time through the tests, set the ulimit for open files
ulimit -n 8192
# Most software is installed automatically by the script, but running the mongo command below from
# the install script was causing some errors. For now this needs to be run manually.
cd installs && curl -sS https://getcomposer.org/installer | php -- --install-dir=bin
cd ..
sudo apt-get remove --purge openjdk-6-jre openjdk-6-jre-headless
mongo --host database-private-ip < config/create.js
```
* Before running the tests, do the following
```bash
$ source ~/.bashrc
```

---

### Installing the Framework Benchmark Database Server

* Install [Ubuntu 14.04](http://www.ubuntu.com/download/server) with username `tfb`
* Log in as `tfb`
* Fully update **NOTE**: If you update the kernel (linux-firmware), it is generally a good idea to reboot aftewards.
```bash
$ sudo apt-get update && sudo apt-get upgrade
```
* Run the command: `sudo visudo`
* Change line 20 in from `%sudo   ALL=(ALL:ALL) ALL` to `%sudo   ALL=NOPASSWD: ALL`
* On the app server, run the following from the FrameworkBenchmark directory (this should only take a small amount of time, several minutes or so):
```bash
$ toolset/run-tests.py --install-software --install database --list-tests
```

---

### Installing the Framework Benchmark Load Server

* Install [Ubuntu 14.04](http://www.ubuntu.com/download/server) with username `tfb`
* Log in as `tfb`
* Fully update **NOTE**: If you update the kernel (linux-firmware), it is generally a good idea to reboot aftewards.
```bash
$ sudo apt-get update && sudo apt-get upgrade
```
* Run the command: `sudo visudo`
* Change line 20 in from `%sudo   ALL=(ALL:ALL) ALL` to `%sudo   ALL=NOPASSWD: ALL`
* On the app server, run the following from the FrameworkBenchmark directory (this should only take a small amount of time, several minutes or so):
```bash
$ toolset/run-tests.py --install-software --install client --list-tests
```

You can validate that the setup worked by running a smoke test like this:

    toolset/run-tests.py --max-threads 1 --name smoketest --test servlet-raw --type all -m verify

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

        powershell -ExecutionPolicy Bypass -Command "iex (New-Object Net.WebClient).DownloadString('https://raw.github.com/TechEmpower/FrameworkBenchmarks/master/toolset/setup/sqlserver/setup-sqlserver-bootstrap.ps1')"

* This will configure SQL Server, the Windows Firewall, and populate the database.

Now, when running `run-tests.py`, just add `-d <ip of SQL Server instance>`. This works for the (Windows Server-based) `aspnet-sqlserver-raw` and `aspnet-sqlserver-entityframework` tests.
