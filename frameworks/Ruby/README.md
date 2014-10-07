# Using RVM to setup your environment

TFB uses rvm wherever possible to help ruby-based or
jruby-based frameworks setup their environment. 
In your framework's `bash_profile.sh`, put this: 

    #!/bin/bash
    
    # Assume single-user installation
    source $HOME/.rvm/scripts/rvm

Because TFB uses Python's `subprocess` module, which runs 
all shell processes in a non-login mode, you must source the 
`rvm` script before using `rvm` anywhere. Placing the `source`
call in your `bash_profile.sh` ensures that it will be called 
before running `install.sh` and before running `setup.py`

For compatibility with how the framework rounds are executed, 
you must use a single-user installation if you wish to run 
ruby-based TFB tests.




