# Vagrant Automated Deployment

Currently, our [Vagrant](https://www.vagrantup.com) setup can automatically
setup a development environment running in a single virtual machine. 
This virtual machine can run inside either [VirtualBox](https://www.virtualbox.org/) or [Amazon EC2](http://aws.amazon.com/ec2/). Feel free to submit a PR 
to help us support more providers, such as [rackspace](http://www.rackspace.com/)
or [VMware](http://www.vmware.com/). 

This single VM runs the `app server`, `client server`, and `database server`, 
and allows you to rapidly test if your framework/framework changes integrate
properly with TFB. While you *can* run a full benchmark using 
this virtual machine, your results would be quite useless. 

## Prerequisites

* **A recent version of Vagrant**, like 1.6.3 (NOTE: `apt-get` is 
too old, download the newest `deb` directly). See 
[here](https://www.vagrantup.com/downloads.html) for downloads.

* **A CPU that can virtualize a 64-bit virtual OS**, because TFB
downloads a number of static binaries that require 64-bit. See
the FAQs section below for more on this

* **VirtualBox**, if you plan to run the Virtual Machine locally. If 
you plan to solely use Amazon-based Virtual Machines you can skip this. 

## Using Vagrant to Run VirtualBox-powered Virtual Machine

In short, you need to clone the project and then run `vagrant up` 
followed by `vagrant ssh`. Your cloned working directory will be 
exposed inside the VM at `~/FrameworkBenchmarks`, so you can 
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
   $ cat results/latest/logs/go/out.txt

You can get lots of help:
   $ toolset/run-tests.py --help

This Vagrant environment is already setup and ready to go, so you
can ignore any flags about users, hosts, or identity files
```

## Using Vagrant to Run Amazon-powered Virtual Machine

The high level steps are similar, run `vagrant up --provider=aws` followed 
by `vagrant ssh`. The main differences are 1) you need to provide a 
number of environment variables to let vagrant log into your AWS account, 
and 2) your git working copy will not be synced with the remote virtual 
machine (by default). 

#### Required Environment Variables for Using Amazon Web Services

These are all required: 

    TFB_AWS_ACCESS_KEY : Your Amazon Web Services Access Key
    TFB_AWS_SECRET_KEY : Your Amazon Web Services Secret Access Key
    TFB_AWS_KEY_NAME   : The name of the keypair you are using
    TFB_AWS_KEY_PATH   : Path to the *.pem file for the keypair you are using

You can declare these in your `~/.bash_profile`, using lines like:

    export TFB_AWS_ACCESS_KEY="<your access key>"

Or you can specify them on the command line as so: 

    $ TFB_AWS_ACCESS_KEY="<your key>" TFB_AWS_SECRET_KEY="<your secret>" TFB_AWS_KEY_NAME="<your keypair name>" TFB_AWS_KEY_PATH="/home/you/.ssh/your_key.pem" vagrant up --provider aws

#### Directoy Syncing and Amazon Web Services

By default, our Vagrant setup will not syncronize your local git working copy 
with the remote filesystem on Amazon. Instead, the `~/FrameworkBenchmarks` 
folder will be a fresh clone of github.com/TechEmpower/FrameworkBenchmarks. 

This is due to the project currently being multiple GB in size (see #1050). 
Synchronizing the local filesystem with the remote filesystem would require an
initial upload to the remote system, and most people do not want to wait for 
a multi-GB upload. 

If you are interested in having this synchronization, you can use set an 
environment variable as so: 

    $ TFB_FORCE_SYNC=true vagrant up --provider aws

This will tell vagrant to keep your git working copy in sync with the remote
filesystem. While the initial upload is quite arduous, any later changes are
synchronized quite quickly (the underlying system is rsync). This would 
allow you to continue editing files on your local machine and then using the 
remote virtual machine to test your edits. As long as you don't run `vagrant 
destroy`, any later synchronization shouldn't take more than a few seconds. 


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

**How much does running on Amazon cost?**: 

Roughly `$1/day` with default settings. 

Running `vagrant up --provider aws` takes about 20 minutes to complete. Running 
`toolset/run-tests.py --install server --mode verify --test <foo>` takes between
5 and 60 minutes, depending on `<foo>`. [Prices as of writing](http://aws.amazon.com/ec2/previous-generation/) show the default `m1.small` instance type is 
`$0.044` per Hour, or roughly `$1/day`

Running `toolset/run-tests.py --mode benchmark --test <foo>` takes between 15-60
minutes per test in `<foo>`, and running the full benchmark would likely take 
between 1 and 5 days of continuous execution. Note that this single VM runs the 
framework, the load generation, and the database, and is therefore not a stable 
benchmarking setup. 