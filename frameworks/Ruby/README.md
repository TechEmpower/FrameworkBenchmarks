# Using RVM to setup your environment

TFB uses rvm wherever possible to help ruby-based or
jruby-based frameworks setup their environment. 
In your framework's `install.sh`, put this prior to other statements: 

    #!/bin/bash
    
    # Assume single-user installation
    source $HOME/.rvm/scripts/rvm

Because TFB uses Python's `subprocess` module, which runs 
all shell processes in a non-login mode, you must source the 
`rvm` script before using `rvm` anywhere. Placing the `source`
call in your `install.sh` before other commands ensures that it 
will be called before installation and before running `setup.py`

For compatibility with how the framework rounds are executed, 
you must use a single-user installation if you wish to run 
ruby-based TFB tests.




