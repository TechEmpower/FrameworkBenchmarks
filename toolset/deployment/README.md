# Toolset Deployment

This directory contains tools and documentation to deploy the benchmark suite on cloud or self-hosted environments.

## Deploment overview

The deployment consists of two phases: provisioning and setup. The provisioning phase varies according to the hosting environment: EC2, dedicated hardware or other hosting. The setup phase is, for the most part, the same for all environments, because the benchmark suite specifies particular versions of operating systems and tools that will be used.

For any hosting option, these are the operating systems and tools that should be provisioned for each role:

#### Linux client

* Runs the benchmark client and the Linux databases (e.g. MySQL, PostgreSQL, MongoDB).
* Operating system: Ubuntu Server 12.04 LTS 64-bit.

#### Linux server

* Hosts the web frameworks under test on a Linux environment.
* Operating system: Ubuntu Server 12.04 LTS 64-bit.

#### Windows server

* Hosts the web frameworks under test on a Windows environment.
* Operating system: Windows Server 2012 Datacenter 64-bit.

#### SQL Server

* Hosts the Microsoft SQL Server database server used by some tests.
* Operating system: Windows Server 2012 Datacenter 64-bit.
* Software: SQL Server 2012 SP1 Standard Edition 64-bit.

## Provisioning instructions

These are the instructions for provisioning the benchmark environment on several hosting alternatives:

### EC2

To deploy the benchmark suite on Amazon Web Services (AWS) Elastic Compute Cloud (EC2):

#### Create the EC2 instances

Create one instance for each role described in the deploment overview above.
* For the Windows server, use the "Microsoft Windows Server 2012 Base" image on Amazon EC2.
* For the SQL Server database server, use the [Windows Server 2012 RTM English 64-bit SQL 2012 Standard](https://aws.amazon.com/amis/amazon-ebs-backed-windows-server-2012-rtm-english-64-bit-sql-2012-standard) image on Amazon EC2.

We tested on m1.large instances, but feel free to experiment with different configurations.

Give the instance that will act as the application server more then the default 8GB of disk capacity (we used 20GB).

#### Security Group

When prompted to create a security group for the instances, here are the ports that you'll need to open:

* 22 (SSH)
* 8080 (Most of the tests run on 8080)
* 3306 (MySQL)
* 5432 (PostgreSQL)
* 9000 (Play Framework)
* 27017 (MongoDB)
* 3000 (yesod)
* 8000 (snap)
* 16969 (cpoll)

### Dedicated hardware

If you have two servers or workstations lying around, then you can install and run the tests on physical hardware.

Please be aware that these setup instructions **can overwrite software and settings**, so it's best to follow these instructions on clean hardware.

#### Evaluation downloads

If you want to run the benchmark on Windows and/or SQL Server, you can download evaluation versions from Microsoft:
* [Download Windows Server 2012](http://technet.microsoft.com/en-us/evalcenter/hh670538.aspx)
* [Download SQL Server 2012 SP1](http://www.microsoft.com/betaexperience/pd/SQL2012EvalCTA/enus/default.aspx)

#### Prerequisites

Before you get started, there are a couple of steps you can take to make running the tests easier on yourself.

Since the tests can run for several hours, it helps to set everything up so that once the tests are running, you can leave the machines unattended and don't need to be around to enter ssh or sudo passwords.

1. Setup an ssh key for the client machine.
2. Edit your sudoers file so that you do not need to enter your password for sudo access.

### Other hosting

The benchmark suite will work on physical or virtual servers that have local network connectivity between them and Internet access for software download.

In general, any private or public hosting can be used, following this procedure:

* Provision the instances (servers) according to the roles, operating system and additional software described in the deploment overview.
* Configure networking as needed.
* Set the Linux hosts for passwordless sudo as described in the instructions for dedicated hardware.

Specific provisioning instructions and/or scripts for particular hosting or cloud services providers are welcome!

## Setup instructions

After the servers are provisioned as described above, execute the setup instructions documented at the [Toolset Setup README file](../setup/README.md).
