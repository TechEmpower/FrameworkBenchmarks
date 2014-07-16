# Windows Azure Deployment

This directory provides guidance and tooling for deploying the Web Framework Benchmarks project on Windows Azure.


## Introduction

Windows Azure is a cloud computing platform and infrastructure, created by Microsoft, for building, deploying and managing applications and services through a global network of Microsoft-managed datacenters. It provides both platform as a service (PaaS) and infrastructure as a service (IaaS) services and supports many different programming languages, tools and frameworks, including both Microsoft-specific and third-party software and systems.

Windows Azure's infrastructure as a service (IaaS) services support both Windows Server and Linux virtual machines. Specifically, Ubuntu is supported through a partnership with Canonical. That makes Windows Azure a viable platform for running the Web Framework Benchmarks suite.


## Architecture

The deployed environment will be composed of:

* An affinity group to keep all resources in the same region.
* A storage account for the virtual disk images.
* A virtual network for connectivity between the VMs and, optionally, secure connection to on-premises networks or individual computers.
* An Ubuntu Server VM for the benchmark client.
* An Ubuntu Server VM for the Linux server.
* A Windows Server VM for the Windows server.
* A Windows Server VM for the Microsoft SQL Server database server.
* A cloud service for each VM for access over the Internet.


## Prerequisites

To deploy the Web Framework Benchmarks suite on Windows Azure you will need:

* A Windows Azure subscription. A [free trial](https://www.windowsazure.com/en-us/pricing/free-trial/) is available.
* Windows Azure Cross-platform Command Line Interface, available for [free download](https://www.windowsazure.com/en-us/downloads/#cmd-line-tools) for Windows, Linux and OS X.
  * On Windows, install the [Cross-platform Command Line Interface](http://go.microsoft.com/?linkid=9828653&clcid=0x409).
  * On Ubuntu, run the bash script `ubuntu-wacli-install.sh` (in the toolset/deployment/azure directory) to install the Windows Azure CLI.
  * On OS X, install the [Command Line Interface](http://go.microsoft.com/fwlink/?linkid=253471&clcid=0x409).
* Unix tools, which are natively available on Linux and OS X, and can be installed on Windows through [Cygwin](http://www.cygwin.com/).
  * On Windows, run the PowerShell script `InstallCygwin.ps1` (in the toolset/deployment/common directory) to install Cygwin. Notice that to Cygwin tools, your `C:\` will be referred to as `/cygdrive/c`, and Cygwin's own directories such as `/home` will be on `C:\Cygwin\home`.


## Instructions

1. Install the prerequisites mentioned above.
2. Download your Windows Azure publish settings file:
  * In a command prompt, run `azure account download`.
  * Log in with your Windows Azure account credentials.
  * Let the browser save the file.
3. Clone the repository:

        git clone https://github.com/TechEmpower/FrameworkBenchmarks.git

4. Create the configuration file `azure-deployment-configuration.sh` as a copy of `azure-deployment-configuration-model.txt`, in the "toolset/deployment/azure" directory.

        bash
        cd ~/FrameworkBenchmarks
        cp toolset/deployment/azure/azure-deployment-configuration-model.txt toolset/deployment/azure/azure-deployment-configuration.sh

5. Edit `azure-deployment-configuration.sh` and configure it according to its embedded documentation.

6. On Linux or OS X, considering the framework was cloned at "~/", execute these commands in a shell:

        cd ~/FrameworkBenchmarks
        nohup toolset/deployment/azure/azure-deployment.sh &> deployment.log &
        tail -f deployment.log

7. On Windows, first install Cygwin running `InstallCygwin.ps1` then, considering the framework was cloned at "C:\", execute these commands in a command prompt:

        C:
        cd \FrameworkBenchmarks
        bash -o igncr toolset/deployment/azure/azure-deployment.sh

The script will provision the resources on Windows Azure and then start the automated deployment of the benchmark suite as described in the [Benchmark Suite Automated Deployment README file](../common/README.md).


## Billing

On Windows Azure, a free trial account and member offers (e.g. MSDN) have by default a spending limit of $0. When your usage exhausts the monthly amounts included in your offer, your service will be disabled for the remainder of that billing month. You have the option to continue using the services by removing the spending limit.

To avoid consuming your credits (in case of an account with spending limit) or incurring in charges (in case of an account without spending limit), you can stop the virtual machines and, optionally, remove their disk images. Virtual machines in state "Stopped (Deallocated)" don't incur in charges. The disk images incur in (relatively small) storage charges.

For more information refer to [Windows Azure Spending Limit](http://www.windowsazure.com/en-us/pricing/spending-limits/) and [Pricing Overview](http://www.windowsazure.com/en-us/pricing/overview/).


## Support
* [Google Groups](https://groups.google.com/forum/?fromgroups=#!forum/framework-benchmarks) for support specific to the Web Framework Benchmarks project.
* [Stack Overflow](http://stackoverflow.com/questions/tagged/azure) and [MSDN Forums](http://social.msdn.microsoft.com/Forums/en-US/category/windowsazureplatform,azuremarketplace,windowsazureplatformctp) for community help on Windows Azure.
* [Windows Azure Support](http://www.windowsazure.com/en-us/support/faq/) for support on Windows Azure by Microsoft. See the available [Options](http://www.windowsazure.com/en-us/support/options/) and [Plans](http://www.windowsazure.com/en-us/support/plans/).
