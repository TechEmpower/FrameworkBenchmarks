# Benchmark Suite Deployment

This directory contains information for deploying the benchmark 
on a range of cloud or self-hosted environments. While you can always use 
*manual deployment*, automated scripts exist for many scenarios. 

Before forging ahead, you should determine if your primary interest is
**development** or **benchmarking**. Once you know this, look at the 
'Summary of Script Directories' section to figure out which directory
has scripts relevant to your use case. 

If your primary interest is **development**, e.g. adding or updating frameworks,
you can use an environment with a single computer running the database, 
the load generation system, and the web framework. We provide scripts for 
configuring a Linux development environment using either Virtualbox or Amazon EC2,
and are actively searching for help adding Windows.

If your primary interest is **benchmarking**, then you will need three computers
(or virtual machines) networked together to fill the three benchmark roles of 
database server, load generation server, and web framework server. We provide 
a Vagrant script to setup a full benchmark-ready Linux environment in Amazon EC2.

If you wish to benchmark a framework on a non-Amazon cloud, e.g. Azure or 
Rackspace, or a framework that requires Windows OS or SQL server, there are 
some limited helper scripts available at the moment. We welcome any pull requests 
along these lines. 

**Please note**: Running this software will make many modifications to software
and settings, so it's recommended you either use a VM or hardware dedicated to this project. 


## Summary of Script Directories


| Directory Name | Summary |
| -------------- | :-------- |
| [azure](azure)  | Scripts to help deploy onto Microsoft Azure
| [common](common) | Common-use scripts that could be used in many environments
| [vagrant-common](vagrant-common) | The core directory for vagrant-based setups. Currently only supports deplconfiguring Linux-based environments
| [vagrant-aws](vagrant-aws) | Setup scripts for configuring Amazon such that you can use `vagrant-development` and `vagrant-production`. Also outlines Amazon-specific options, such as instance type, you can use with `vagrant-development` and `vagrant-production`
| [vagrant-virtualbox](vagrant-virtualbox) | A readme defining all the Virtualbox-specific options you can use with `vagrant-development` and `vagrant-production`, such as amount of RAM per virtual machine
| [vagrant-development](vagrant-development) | Sets up a development environment using a single Virtual Machine. Can use either Virtualbox (for a free, locally run virtual machine) or Amazon EC2 (for a `$1/day` remote virtual machine)
| [vagrant-production](vagrant-production) | Sets up a 3-virtual machine environment in either Amazon or Virtualbox. If Amazon is used, this is intended to be an environment you could run an official `TechEmpower` round with. A 3-VM VirtualBox setup is mainly used to test-run in a free environment before launching into a paid environment


## Manual Deployment


**Evaluation downloads**

If you want to run the benchmark on Windows and/or SQL Server, you can download evaluation versions from Microsoft:
* [Download Windows Server 2012](http://technet.microsoft.com/en-us/evalcenter/hh670538.aspx)
* [Download SQL Server 2012 SP1](http://www.microsoft.com/betaexperience/pd/SQL2012EvalCTA/enus/default.aspx)

**Prerequisites**

Before you get started, there are a couple of steps you can take to make running the tests easier on yourself.

Since the tests can run for several hours, it helps to set everything up so that once the tests are running, you can leave the machines unattended and don't need to be around to enter ssh or sudo passwords.

1. Setup an ssh key for the client machine.
2. Edit your sudoers file so that you do not need to enter your password for sudo access.

## Other hosting

The benchmark suite will work on physical or virtual servers that have local network connectivity between them and Internet access for software download.

In general, any private or public hosting can be used, following this procedure:

* Provision the instances (servers) according to the roles, operating system and additional software described in the deploment overview.
* Configure networking as needed.
* Set the Linux hosts for passwordless sudo as described in the instructions for dedicated hardware.

Specific provisioning instructions and/or scripts for particular hosting or cloud services providers are welcome!

## Setup

After the servers are provisioned as described above, it's time to deploy the benchmark suite and its dependencies.

* **Automated deployment:** To use the automated deployment script, refer to the [Benchmark Suite Automated Deployment README file](common/README.md).
* **Manual setup:** To manually execute the deployment steps, refer to the [Benchmark Suite Setup README file](../setup/README.md).
