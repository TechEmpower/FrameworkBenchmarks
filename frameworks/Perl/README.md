# Installation and Bash Configuration

In order to declare that your framework requires Perl, you should have an `install.sh`
that contains at least

    #!/bin/bash
    export PERL_HOME=${IROOT}/perl-5.18
    export PATH="$PERL_HOME/bin:$PATH"

    fw_depends perl

This installs the Perl interpreter and some basic modules (see Dependency Management).
The `install.sh` file should then install any required perl modules or other supporting
applications.

Perl is installed in the `$IROOT` directory. Currently, the Perl interpreter
used by the test is in version 5.18 family. This will provide the `$PERL_HOME` path - should you need it - and 
allow all apps installed by Perl to be used directly.

# Dependency Management

While installing Perl, the [`cpanm`](https://metacpan.org/pod/distribution/App-cpanminus/bin/cpanm)
client is installed as well as the more advanced [`Carton`](https://metacpan.org/pod/Carton).
Carton can be used to declare and use project specific dependencies with more fine grained control
of versions and even pin the specific versions used during development.

# Where to get help

Perl advice can be found in #perl on freenode or on any number of channels on irc.perl.org.
Frameworks might declare specific app maintainers in their app's README files.

