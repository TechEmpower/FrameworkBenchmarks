# Setting up a Development Environment Using Vagrant

The standard development environment is a single computer that fills the 
role of database server, load generation server, and framework server. 
While this is not useful for serious benchmarking, it does allow 
painless development and testing. Using a virtual machine as a 
development environment prevents having to install hundreds of 
framework dependencies directly onto your computer. 

We provide a [Vagrant](https://www.vagrantup.com) script to automatically 
setup a virtual machine as a development server. Currently, you can use 
either [VirtualBox](https://www.virtualbox.org/) or [Amazon EC2](http://aws.amazon.com/ec2/)
to run this development environment. This page details how to use 
VirtualBox, please read [here](blah) for information on using Amazon.

Feel free to submit a PR to help us support more providers, such as 
[Rackspace](http://www.rackspace.com/) or [VMware](http://www.vmware.com/). 

## Prerequisites

* **A recent version of Vagrant**, like 1.6.3 (NOTE: `apt-get` is 
too old, download the newest `deb` directly). See 
[here](https://www.vagrantup.com/downloads.html) for downloads

* **A CPU that can virtualize a 64-bit virtual OS**, because TFB
downloads a number of static binaries that require 64-bit. See
the FAQs section below for more on this. If you cannot meet this 
requirement, consider using the Amazon provider (about `$1/day`)

* **VirtualBox** to run the virtual machines

## Launching VirtualBox Development Environment

In short, you need to clone the project and then run `vagrant up` 
followed by `vagrant ssh`. Your cloned working directory will be 
exposed inside the virtual machine at `~/FrameworkBenchmarks`, so you can 
continue to edit files using your favorite IDE and the changes will
show up inside the virtual machine. 

Details: 

```bash
# Go into the right directory
$ cd FrameworkBenchmarks/toolset/deployment/vagrant
# Setup the VM with all initial software (takes 15-20 minutes)
$ vagrant up
# SSH into the machine (you get a nice welcome message)
$ vagrant ssh
Welcome to the FrameworkBenchmarks project!
   
To get started, perhaps try this:
   $ cd FrameworkBenchmarks
   $ toolset/run-tests.py --install server --test go
   $ cat results/ec2/latest/logs/go/out.txt

You can get lots of help:
   $ toolset/run-tests.py --help

This Vagrant environment is already setup and ready to go, so you
can ignore any flags about users, hosts, or identity files
```

## Using VirtualBox Development Environment

After using `vagrant ssh`, you will find the project at `~/FrameworkBenchmarks`. 
You can use `run-tests.py` without specifying any information about hosts, 
usernames, or private key files, as so: 

    $ cd ~/FrameworkBenchmarks
    $ toolset/run-tests.py --mode verify --install server --test go
    <log for installing software needed for go test>
    <log for launching go framework>
    <log for verifying go framework meets requirements>
    $ cat results/ec2/latest/logs/go/out.txt
    <log with stdout from framework go>

# Using Amazon-powered Virtual Machine

Please read the document [here]

The high level steps are similar, run `vagrant up --provider=aws` followed 
by `vagrant ssh`. The main differences are 1) you need to provide a 
number of environment variables to let vagrant log into your AWS account, 
and 2) your git working copy will not be synced with the remote virtual 
machine (by default). 

### FAQs

**I'm using a 32-bit computer, can I run your Virtual Machine?**: 

If 1) your CPU provides the [vmx or smv](http://en.wikipedia.org/wiki/X86_virtualization) features, and 2) they are enabled
in your BIOS and 3) they are enabled in Virtualbox, then yes. 
The easiest way to check this all is to attempt to launch the VM 
without hiding the window:

    $ TFB_SHOW_VM=true vagrant up

If you see a boot sequence and a login, you're good to go. If you 
only see a black screen, examine if you meet all three requirements. 
If you do not, then you can either 1) run on Amazon or 2) try your
luck with a 32-bit virtual machine. A number of the downloaded 
framework binaries are 64-bit only, so those will be unable to run. 

To force FrameworkBenchmarks to use a 32-bit VM, do this: 
     
    $ TFB_ARCH=32 vagrant up

See [here](http://askubuntu.com/questions/41550) for some helpful information.  
