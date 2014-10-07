# Automated Deployment Onto Amazon Web Services

Scripts exist to setup either a development environment, or a benchmark-ready
production environment. The development environment uses one small instance to 
run all three TFB roles (app server, database server, client load generation). 
The production environment uses three different instances networked together. 
There are a number of environmental variables listed below that allow you to 
change features such as instance type, availability region, etc. 

We use [Vagrant](https://www.vagrantup.com) to launch/terminate EC2 instances, 
and we provide a python script to configure the network as needed. 

All amazon resources are tagged with `Project=FrameworkBenchmarks` and 
`TFB_Role=[database|network|client|app]` so you can easily get a breakdown on 
your bill. A default development setup costs about `$1/day`, while a default
production setup costs about TODO. 

## Prerequisites

* **A recent version of Vagrant**, like 1.6.3 (NOTE: `apt-get` is 
too old, download the newest `deb` directly). See 
[here](https://www.vagrantup.com/downloads.html) for downloads

* **Vagrant AWS Plugin** from [here](https://github.com/mitchellh/vagrant-aws)

* **Python 2.7** to run the amazon setup script

* **Amazon Command Line Interface**: From [here](https://github.com/aws/aws-cli)

## Using Vagrant to Run Amazon-powered Virtual Machine

The high level steps are 
1) clone this project 
2) set environment variables allowing us to log into your amazon account
3) Run amazon setup script to create network
4) Run `vagrant up --provider=aws` to launch into amazon
5) Run `vagrant ssh` to log into the application server

By default, your local git clone of this project will not by synced with the 
remote amazon machines.

## Required Environment Variables

These environment variables are always required: 

    TFB_AWS_ACCESS_KEY : Your Amazon Web Services Access Key
    TFB_AWS_SECRET_KEY : Your Amazon Web Services Secret Access Key
    TFB_AWS_KEY_NAME   : The name of the keypair you are using
    TFB_AWS_KEY_PATH   : Path to the *.pem file for the keypair you are using

You can declare these in your `~/.bash_profile`, using lines like:

    export TFB_AWS_ACCESS_KEY="<your access key>"

Or you can specify them on the command line as so: 

    $ TFB_AWS_ACCESS_KEY="<your key>" TFB_AWS_SECRET_KEY="<your secret>" TFB_AWS_KEY_NAME="<your keypair name>" TFB_AWS_KEY_PATH="/home/you/.ssh/your_key.pem" vagrant up --provider aws

## Directory Syncing

By default, our Vagrant setup will not syncronize your local git working copy 
with the remote filesystem on Amazon. Instead, the `~/FrameworkBenchmarks` 
folder will be a fresh clone of `github.com/TechEmpower/FrameworkBenchmarks`

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

## Environmental Variables 

These only have effect if the vagrant aws provider is used. See 
[here](../vagrant-virtualbox) for variables that take effect if the 
virtualbox provider is used. 

| Name                             | Values              | Purpose                  |
| :------------------------------- | :------------------ | :----------------------- | 
| <sup>1</sup>`TFB_AWS_ACCESS_KEY` | `<your_key>`        |  Your Amazon Web Services Access Key 
| <sup>1</sup>`TFB_AWS_SECRET_KEY` | `<your_key>`        |  Your Amazon Web Services Secret Access Key 
| <sup>1</sup>`TFB_AWS_KEY_NAME`   | `<key_name>`        |  The name of the key pair you are using 
| <sup>1</sup>`TFB_AWS_KEY_PATH`   | `<path_to_keyfile>` |  Path to the *.pem file for the keypair you are using 
| `TFB_FORCE_SYNC`                 | `true,false`        | Tells vagrant to rsync your local git clone with the remote amazon instance. Note: This requires an initial multi-GB upload 
| `TFB_AWS_REPO_SLUG`              | default is `TechEmpower/FrameworkBenchmarks`  | If `TFB_FORCE_SYNC` is false, this defines the repository that will be checked out from Github on the Amazon instances
| `TFB_AWS_REPO_BRANCH`            | default is `master`  | If `TFB_FORCE_SYNC` is false, this defines the repository branch that will be checked out from Github on the Amazon instances
| `TFB_AWS_EC2_TYPE`               | `m1.small,c1.xlarge,etc` | The instance type used. Requires [previous generation instance](http://aws.amazon.com/ec2/previous-generation/) names because vagrant-aws does not yet support the new names
| <sup>2</sup>`TFB_AWS_SUBNET`     | `<subnet_id>` (e.g. `subnet-2737230f`) | Which subnet instances should be launched into. The python setup script can generate this value for you
| <sup>2</sup>`TFB_AWS_SEC_GROUP`  | `<security_group_id>` (e.g. `sg-871240e2`) | The default security group instances use
| `TFB_AWS_EBS_TYPE`               | `gp2,standard,io1`  | The EBS [type](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#block-device-mapping-def). `gp2` is a general-purpose SSD, `standard` is a magnetic drive, `io1` is a guaranteed I/O SSD drive. Our benchmark mode defaults to `gp2`
| `TFB_AWS_EBS_IO`                 | `<number>`          | Only used if `TFB_AWS_EBS_TYPE=io1`. The number of IO operations per second
| `TFB_AWS_EBS_DELETE`             | `true,false`        | Should the EBS volume be deleted when `vagrant destroy` happens? Note: We cannot currently tag EBS with `Project=FrameworkBenchmarks`, so you'll need to remember which volumes were from this project, and which of those was the application server volume
| `TFB_AWS_APP_IP`                 | `<ip address>`      | IP address used inside Amazon for the application server. Default is `172.16.0.16`
| `TFB_AWS_LOAD_IP`                | `<ip address>`      | IP address used inside Amazon for the application server. Default is `172.16.0.17`
| `TFB_AWS_DB_IP`                  | `<ip address>`      | IP address used inside Amazon for the database server. Default is `172.16.0.18`
| `TFB_AWS_REGION`                 | `<aws region>`      | Region to launch the instances in. Default is `us-east-1`

<sup>1</sup> Variable is required

<sup>2</sup> Variable can be auto-generated using `setup_aws.py`

**Note:** We do not currently support setting instance-specific variables, like instance type 
or EBS variables, on a per-instance basis. The only exception is the ip address used for the 
instances. 

### Amazon Tips and Tricks

**Halt Amazon instances to save money**

You can use `vagrant stop` with the AWS provider, and using 
`vagrant up` at a later date to restart the virtual machine. You will 
continue to be charged for the block storage (about $0.05 per month per 
GB), but you will not be charged for the CPU required to run the VM (about
$5 per month). With our normal setup, this block storage cost is about 
$0.40 per month (we currently use an 8GB volume)

**Use Disk Snapshots For Rapid Launching**

If you hate waiting 20 minutes for the virtual machine to initially
be ready, just snapshot it after the first `vagrant up`. You'll have to 
pay around $0.10 per month per GB to maintain this snapshot, but you can 
use the snapshot as the disk image when starting a new virtual machine and 
have a AWS development environment ready in seconds. 

This trick is of limited utility if you are just doing the initial setup, 
but it can become very valuable if you have run a **full** installation of 
server software (e.g. `toolset/run-tests.py --install server --install-only`), 
as this takes multiple hours to complete. 

### FAQs

**How much does running on Amazon cost?**: 

Roughly `$1/day` with default settings.  

Running `vagrant up --provider aws` takes about 20 minutes to complete. Running 
`toolset/run-tests.py --install server --mode verify --test <foo>` takes between
5 and 60 minutes, depending on `<foo>`. [Prices as of writing](http://aws.amazon.com/ec2/previous-generation/) show the default `m1.small` instance type is 
`$0.044` per Hour, or roughly `$1/day`. You are charged for a full hour even
if you only use a few minutes, so if you contantly cycle between `vagrant up`
and `vagrant destroy` you will be charged `$0.044` per cycle. 

Running `toolset/run-tests.py --mode benchmark --test <foo>` takes between 15-60
minutes per test in `<foo>`, and running the full benchmark would likely take 
between 1 and 5 days of continuous execution. Note that this single VM runs the 
framework, the load generation, and the database, and is therefore not a stable 
benchmarking setup. 

**Are spot instances supported?**:

There is an open issue at [mitchellh/vagrant-aws#32](https://github.com/mitchellh/vagrant-aws/issues/32) that appears to have 
working code for spot instances. This could reduce the amazon cost up to 100x!
Once this is supported in vagrant-aws it will be supported here. 

**I disagree with the Amazon Setup!**:

You're free to submit a PR to improve the automated deployment scripts. Please
justify why you feel that your changes provide an improvement, and explain the
total cost of running a benchmark with your update setup. Be aware that 
you can use `vagrant provision` to selectively re-run your provision scripts,
instead of having to constantly terminate and launch instances. 

**Can I use VirtualBox and AWS Simultaneously?**:

Not out of the box. Vagrant does not currently support using two 
providers simultaneously for the same Vagrantfile. The quick fix is to 
just copy the folder, and run one provider in one folder and one provider
in another. 

**Can I run multiple benchmarks simultaneously?**:

Sure, but you will need to duplicate the directory containing the
`Vagrantfile` and you will also need to use `TFB_AWS_APP_IP` (and similar 
variables) to avoid IP address conflicts. You could re-run the `setup_aws.py`
script and avoid changing the IP addresses, but this creates a new 
Virtual Private Cloud (VPC), and you are limited to 5 VPC's per region. If 
you are reading this FAQ, you may be interested in running more than 5 
simultaneous benchmarks, so the better approach is to just increase all
the IP addresses by 3 and run the additional benchmarks in the same VPC. 


**I'm getting an AuthFailure but my Credientials are Correct!**:

This normally means the AMI has been moved from public to private. Ubuntu's 
Cloud image team occasionally does this. Navigate [here](http://cloud-images.ubuntu.com/trusty/current/) and find a more current AMI. 
