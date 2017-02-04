# Automated Deployment Onto DigitalOcean

Vagrant scripts exist to use DigitalOcean as a development environment. 
You'll need to have the [Vagrant plugin for DigitalOcean](https://github.com/smdahlen/vagrant-digitalocean) installed, and have some environment vars configured to let Vagrant create droplets on your behalf. 

## Prerequisites

* **A recent version of Vagrant**, like 1.6.3 (NOTE: `apt-get` is 
too old, download the newest `deb` directly). See 
[here](https://www.vagrantup.com/downloads.html) for downloads

* **Vagrant DigitalOcean Plugin** from [here](https://github.com/smdahlen/vagrant-digitalocean)

## Using Vagrant to Run DigitalOcean-powered Droplets

**Note: For DigitalOcean, you must use --no-provision with up!** This 
[bug report](https://github.com/smdahlen/vagrant-digitalocean/issues/183) details why. 

The high level steps are 
1) clone this project 
2) set environment variables allowing us to log into your DigitalOcean account
3) Run `vagrant up --provider=digital_ocean --no-provision` to launch a droplet
4) Run `vagrant provision` to setup the droplet
5) Run `vagrant ssh` to log into the application server

By default, your local git clone of this project will not by synced with the 
remote DigitalOcean droplets.

## Required Environment Variables

These environment variables are required:  

    TFB_DO_TOKEN   : Your Digital Ocean Personal Access Token'

You can declare these in your `~/.bash_profile`, using lines like:

    export TFB_DO_TOKEN="<your token>"

Or you can specify them on the command line as so: 

    $ TFB_DO_TOKEN="<your token>" vagrant up --provider digital_ocean

## Directory Syncing

By default, our Vagrant setup will not syncronize your local git working copy 
with the remote filesystem on DigitalOcean. Instead, the 
`~/FrameworkBenchmarks` folder will be a fresh clone of `github.com/TechEmpower/FrameworkBenchmarks`

This is due to the project currently being multiple GB in size (see #1050). 
Synchronizing the local filesystem with the remote filesystem would require an
initial upload to the remote system, and most people do not want to wait for 
a multi-GB upload. 

If you are interested in having this synchronization, you can use set an 
environment variable as so: 

    $ TFB_FORCE_SYNC=true vagrant up --provider digital_ocean

This will tell vagrant to keep your git working copy in sync with the remote
filesystem. While the initial upload is quite arduous, any later changes are
synchronized quite quickly (the underlying system is rsync). This would 
allow you to continue editing files on your local machine and then using the 
remote droplet to test your edits. As long as you don't run `vagrant 
destroy`, any later synchronization shouldn't take more than a few seconds. 

## Environmental Variables 

These only have effect if the DigitalOcean provider is used. See 
[here](../vagrant-virtualbox) for variables that take effect if the 
virtualbox provider is used. 

| Name                             | Values              | Purpose                  |
| :------------------------------- | :------------------ | :----------------------- | 
| <sup>1</sup>`TFB_DO_TOKEN`           | `<your_key>`        |  Your DigitalOcean Personal Access Token 
| `TFB_DO_PRIVATE_KEY`           | `~/.ssh/id_rsa`        |  Path to the private key file you will use to login to your droplet
| `TFB_FORCE_SYNC`                 | `true,false`        | Tells vagrant to rsync your local git clone with the remote droplet. Note: This requires an initial multi-GB upload 
| `TFB_REPO_SLUG`              | default is `TechEmpower/FrameworkBenchmarks`  | If `TFB_FORCE_SYNC` is false, this defines the repository that will be checked out from Github on the remote droplet
| `TFB_REPO_BRANCH`            | default is `master`  | If `TFB_FORCE_SYNC` is false, this defines the repository branch that will be checked out from Github on the remote droplet
| `TFB_DO_IMAGE`                      | `ubuntu-14-04-x64, etc` | DigitalOcean image name
| `TFB_DO_REGION`                     | `nyc2,nyc1, etc` | DigitalOcean region
| `TFB_DO_SIZE`                       | `512mb,1gb`   | Size of image RAM, which also determines available SSD space, transfer, and processing. See [here](https://www.digitalocean.com/pricing/)

<sup>1</sup> Variable is required

### DigitalOcean Tips and Tricks

**Resize droplets to temporarily boost CPU and RAM**

DigitalOcean allows you to temporarily resize your droplet. This allows you to quickly increase your CPU and RAM for a short period of time. See [here](https://www.digitalocean.com/community/tutorials/how-to-resize-your-droplets-on-digitalocean)

**Avoid Destroying Droplets For Rapid Launching**

You can use `vagrant stop` to power off your droplet. You will continue to be charged, as the droplet continues to utilize disk space. However, for the default size this charge is less than one penny per hour. Using 
`vagrant up` at a later date will restart the droplet, which you can then use instantly without having to run the full setup proceduce. 

This trick is of limited utility if you are just doing the initial setup, 
but it can become very valuable if you have run a **full** installation of 
server software (e.g. `toolset/run-tests.py --install server --install-only`), 
as this takes multiple hours to complete. 

### FAQs

**How much does running on Digital Ocean cost?**: 

Roughly `$0.17/day` with default settings.  

Running `vagrant up --provider digital_ocean` takes about 20 minutes to complete. Running 
`toolset/run-tests.py --install server --mode verify --test <foo>` takes between
5 and 60 minutes, depending on `<foo>`. [Prices as of writing](https://www.digitalocean.com/pricing/) show the default `512mb` instance type is 
`$0.007` per Hour, or roughly `$0.17/day`. You are charged for a full hour even
if you only use a few minutes, so if you contantly cycle between `vagrant up`
and `vagrant destroy` you will be charged `$0.007` per cycle. 

Running `toolset/run-tests.py --mode benchmark --test <foo>` takes between 15-60
minutes per test in `<foo>`, and running the full benchmark would likely take 
between 1 and 5 days of continuous execution. Note that this single VM runs the 
framework, the load generation, and the database, and is therefore not a stable 
benchmarking setup. 

**Can I use VirtualBox and DigitalOcean Simultaneously?**:

Not out of the box. Vagrant does not currently support using two 
providers simultaneously for the same Vagrantfile. The quick fix is to 
just copy the folder, and run one provider in one folder and one provider
in another. 

