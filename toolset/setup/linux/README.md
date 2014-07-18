# Linux Installer Overview

The `installer.py` file calls into the shell to perform all 
installation steps. It installs three different sets of software, the 
server software we want to test, the database that we want to use with 
the server software, and the client software that we want to use to 
generate load for the server. This file gives an overview of how this 
installation process works

## Using Install Server Flag

To trigger a server installation, run `toolset/run-tests.py --install server` 
(or `--install all`). Note: Software is installed onto the current machine, 
you should run the server installation on the machine you plan to run 
servers on during benchmarking. This requires password-less sudo access 
for the current user on the current system. 

The software needed depends upon what tests are going to be performed--clearly
if you only wish to test the `go` framework, there is no need to install 
`php`. To avoid installing all server software all the time, you can use 
the `--test` and `--exclude` flags in conjunction with `--install server`. 
Here are some examples: 

```bash
$ # Always run from the root directory of the project
$ cd ~/FrameworkBenchmarks
$ # Install server software to run go
$ toolset/run-tests.py --install server --test go
$ # Install server software for go, install client and database software too
$ toolset/run-tests.py --install all --test go
$ # Install server software for all tests
$ toolset/run-tests.py --install server
$ # Install all software for all tests
$ toolset/run-tests.py --install all
```

Some prerequisite software is needed regardless of what test is being
installed. This includes core tools such as `build-essential`, `curl`, etc.
To install just the prerequisites, set the `--test` flag to empty, as so:

```bash
$ # Install just prerequisite server software
$ toolset/run-tests.py --install server --test ''
```

### Understanding Installation Strategy

When possible, TFB installs all software into isolated folders underneath
the `installs/` directory. This helps to prevent different frameworks from 
interfering with each other. There are two strategies used to 
determine which folder software will be installed into, `pertest` or 
`unified`. With `--install-strategy unified`, all software will be 
installed into folders underneath `installs/`. If multiple frameworks depend
on the same software, it will not be installed twice. This can have large 
benefits for download and compile time, e.g. downloading and compiling 
`php` twice is substantially more work than once. However, it also means that
frameworks are not guaranteed complete isolation - if a framework somehow 
modifies it's installation, then other frameworks using that same dependency
will be affected. 

`--install-strategy unified` results in a directory structure like this: 

```
installs
├── go
├── nodejs
├── php
└── py2
```

With `--install-strategy pertest`, each framework has an isolated installation 
directory and is guaranteed to have it's own copy of installation. This takes
substantially more time for some tests, as large dependencies must be downloaded,
decompressed, and compiled multiple times. 

`--install-strategy pertest` results in a directory structure like this: 

```
installs
└── pertest
    ├── aspnet
    │   └── mono
    ├── aspnet-stripped
    │   └── mono
    ├── dart
    │   ├── go
    │   ├── nodejs
    │   ├── php
    │   └── py2
    └── go
        ├── go
        ├── nodejs
        ├── php
        └── py2
```

Note that both installation strategies use the `installs/` directory, so 
you can always delete this directory to remove all software that was able
to be installed into folders. There is no simple uninstallation strategy for 
software that is installed into standard system folders. 

### How Server Installation Works (Python)

The function `Installer#__install_server_software` is called to install
server software. Here are the steps it follows: 

1. Use `--test` and `--exclude` flags to decide which frameworks need installation. For each test, do the following: 
2. If this test has a `bash_profile.sh` file, load the environment from it
3. Find installation directory for this test, set environment variable `IROOT`
4. Find root directory for this test, set environment variable `TROOT`
5. Load the functions from `toolset/setup/linux/bash_functions.sh`
6. Execute the `install.sh` for this test. This normally uses functions 
defined in `bash_functions.sh`, such as `fw_depends`

### How Server Installation Works (Bash)

Each framework directory has two files to customize how the installation is
run, `install.sh` and `bash_profile.sh`. To go over each: 

The `install.sh` file for each framework starts the bash process which will 
install that framework. Typically, the first thing done is to call `fw_depends` 
to run installations for any necessary software that TFB has already 
created installation scripts for. TFB provides a reasonably wide range of 
core software, so your `install.sh` may only need to call `fw_depends` and 
exit. Note: `fw_depends` does not guarantee dependency installation, so 
list software in the proper order e.g. if `foo` depends on `bar`
use `fw_depends bar foo`

Here are some example `install.sh` files

```bash
#!/bin/bash

# My server only needs nodejs
fw_depends nodejs
```

```bash
#!/bin/bash

# My server is weird and needs nodejs and mono and go
fw_depends nodejs mono go
```

```bash
#!/bin/bash

# My server needs nodejs...
fw_depends nodejs mono go

# ...and some other software that there is no installer script for.
# Note: Use IROOT variable to put software in the right folder
# Please see guidelines on writing installation scripts
wget mystuff.tar.gz -O mystuff.tar.gz
untar mystuff.tar.gz
cd mystuff
make --prefix=$IROOT && sudo make install
```

The `bash_profile.sh` file is sourced before installing software or before
running the framework test. This is mostly used when running your 
framework, to perform actions such as updating `PATH` or defining environment 
variables your framework requires e.g. `GOROOT`. It is unlikely you need to 
reference these variables in your `install.sh`, but they are 
available. **Only** put variables in `bash_profile.sh` if they are needed
for running your software. If you only need variables for installation, just 
define them in `install.sh`

Example of `bash_profile.sh`. All of these variables will be available for 
use inside `install.sh`, if they are needed. 

```bash
# Set the root of our go installation
export GOROOT=${IROOT}/go

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

export GOPATH=${FWROOT}/go
```

### Available Dependency Installation Scripts (for fw_depends)

To see what TFB provides installations for, look in `toolset/setup/linux`
in the folders `frameworks`, `languages`, `systools`, and `webservers`. 
You should pass the filename, without the ".sh" extension, to fw_depends. 
Here is a listing as of July 2014: 

```bash
$ ls frameworks                                                                
grails.sh  nawak.sh  play1.sh  siena.sh     vertx.sh  yesod.sh
jester.sh  onion.sh  play2.sh  treefrog.sh  wt.sh
$ ls languages
composer.sh  erlang.sh   hhvm.sh   mono.sh    perl.sh     pypy.sh     racket.sh   urweb.sh
dart.sh      go.sh       java.sh   nimrod.sh  phalcon.sh  python2.sh  ringojs.sh  xsp.sh
elixir.sh    haskell.sh  jruby.sh  nodejs.sh  php.sh      python3.sh  ruby.sh     yaf.sh
$ ls systools
leiningen.sh  maven.sh
$ ls webservers
lapis.sh  mongrel2.sh  nginx.sh  openresty.sh  resin.sh  weber.sh  zeromq.sh
```

### Guidelines for Writing Installation Scripts

Whether you are writing new dependency files (e.g. a new language installation
script) or just appending custom code to your framework's `install.sh`, here 
are some guidelines for proper use.

0. Familiarize yourself with the functions available in `bash_functions.sh`
1. **Use caching whenever possible** : Use `fw_exists` to avoid re-running 
installations. Note: If you are in the process of writing a new installation, 
you may wish to delete the file checked by `fw_exists` to force an installation 
to run
2. **Protect against Ctrl-C and partial installations**: Only use `fw_exists`
on objects that exist if the entire installation completed, such as binaries. 
If you use `fw_exists` on a downloaded file, there is no guarantee that 
installation completed. If you use `fw_exists` on an installation directory,
there is no guarantee that compilation completed. Note: Another approach is 
to run your entire compilation, and then move your completed installation to 
a new directory and `fw_exists` on this new directory. 
3. **Specify download file locations**: Use `wget -O` and similar curl options
to avoid having "file.1","file.2", etc when you expect to always have "file"
4. **Understand running bash scripts with errtrace option**: This is likely 
the hardest thing. *If any command in your script returns non-zero, TFB will 
report a potential installation error!* This means no `return 1` statements
except to indicate installation failure, no `mv foo bar` if you're not 
100% sure that foo exists and can be moved to bar, etc. Note that the bash
or operator (||) can be used to avoid errtrace from inspecting a command, 
so you can use something like `mv foo bar || true` to avoid having errtrace
inspect the exit code of the `mv` command. 
5. **Install into folders**: As mentioned above, please try to install your
software into folders. Your installation script will always have the `$IROOT`
variable available, use this to determine what folder your software should
be installed into e.g. `ME=$IROOT/my-software`
6. **Turn on debugging if you're stuck**: `bash_functions.sh` has an ERR
trap inside it. It's fairly useful, but if you would like more information 
you can uncomment some additional lines in the ERR trap to cause it to print 
an entire bash stack trace e.g. what command in what function in what file on 
what line caused my non-zero status. This is useful, but beware that dragons 
are involved in reading bash stack traces...
7. **Look at the examples!!**: There are tons of example installation scripts 
inside of `toolset/setup/linux`, so please examine them

### Guidelines for Using bash_profile.sh

Only one guideline really...don't output any information to stdout! This file 
should only be used for declaring environment variables e.g. `export FOO=bar`. 
Anything you print to stdout or stderr we will ignore (or try to!), and 
you won't see this output in your console.

### Bash Variables Available 

* FWROOT: Root of project
* IROOT: Root of installation for the current framework
* TROOT: Root directory for the current framework 

## Database Installation

The `Installer#__install_database` function uses SFTP to copy a number of 
files to the remote database system, and then uses SSH to log in and 
execute the installation. 

This requires password-less sudo access for the `--database-user` on the 
remote system `--database-host`. 

## Client Installation

The `Installer#__install_client_software` function uses SFTP to copy a number of 
files to the remote system, and then uses SSH to log in and 
execute the installation. 

This requires password-less sudo access for the `--client-user` on the 
remote system `--client-host`. 
