import re
import os
import subprocess

# Replaces all text found using the regular expression to_replace with the supplied replacement.
def replace_text(file, to_replace, replacement):
    with open(file, "r") as conf:
        contents = conf.read()
    replaced_text = re.sub(to_replace, replacement, contents)
    with open(file, "w") as f:
        f.write(replaced_text)

# Replaces the current process environment with the one found in 
# config file. Retains original HOME/PATH/USER by default (although)
# this can be overwritten by config if desired
#
# Note: This will not work if the command you are running from python
# starts with sudo (e.g. subprocess.check_call("sudo <command>")). 
# If you must do this, consider sudo sh -c ". <config> && your_command"
#
# This will work if you run a command that internally calls sudo, 
# as long as your /etc/sudoers correctly has the NOPASSWD option set
def replace_environ(config=None, root=None, print_result=False, command='true'):
    # Source file and print resulting environment
    setup_env = "%s && . %s && env" % (command, config)
    mini_environ = os.environ.copy()
    mini_environ.clear()
    if 'HOME' in os.environ.keys():
        mini_environ['HOME']=os.environ['HOME']
    if 'PATH' in os.environ.keys():
        mini_environ['PATH']=os.environ['PATH']
    if 'USER' in os.environ.keys():
        mini_environ['USER']=os.environ['USER']
    if 'LD_LIBRARY_PATH' in os.environ.keys():
        mini_environ['LD_LIBRARY_PATH'] = os.environ['LD_LIBRARY_PATH']
    if 'PYTHONPATH' in os.environ.keys():
        mini_environ['PYTHONPATH'] = os.environ['PYTHONPATH']

    if root is not None: 
      mini_environ['FWROOT']=root
    elif 'FWROOT' in os.environ.keys():
        mini_environ['FWROOT']=os.environ['FWROOT']
    
    os.environ.clear()
    env = subprocess.check_output(setup_env, shell=True, env=mini_environ,
      executable='/bin/bash')
    for line in env.split('\n'):
        try:
            split=line.index('=')
            key=line[:split]
            value=line[split+1:]
            os.environ[key]=value
        except:
            if not line: # Don't warn for empty line
                continue 
            print "WARN: Line '%s' from '%s' is not an environment variable" % (line, config)
            continue
    if print_result:
        out = subprocess.check_output('env', shell=True, executable='/bin/bash')
        print "Environment after loading %s" %config
        print out
