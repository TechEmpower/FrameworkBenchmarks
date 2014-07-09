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
# config file. Retains a few original vars (HOME,PATH, etc) by default. 
# Optionally allows specification of a command to be run before loading
# the environment, to allow the framework to set environment variables
# Note: This command *cannot* print to stdout!
#
# Note: This will not replace the sudo environment (e.g. subprocess.check_call("sudo <command>")). 
# If you must use sudo, consider sudo sh -c ". <config> && your_command"
def replace_environ(config=None, root=None, print_result=False, command='true'):
    # Clean up our current environment, preserving some important items
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

    # Run command, source config file, and store resulting environment
    setup_env = "%s && . %s && env" % (command, config)
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

# Queries the shell for the value of FWROOT
def get_fwroot():
    try:
        # Use printf to avoid getting a newline
        # Redirect to avoid stderr printing
        fwroot = subprocess.check_output('printf $FWROOT 2> /dev/null', shell=True, executable='/bin/bash')
        return fwroot
    except subprocess.CalledProcessError:
        return "";

# Turns absolute path into path relative to FWROOT
# Assumes path is underneath FWROOT, not above
# 
# Useful for clean presentation of paths 
# e.g. /foo/bar/benchmarks/go/bash_profile.sh
# v.s. FWROOT/go/bash_profile.sh 
def path_relative_to_root(path):
    # Requires bash shell parameter expansion
    return subprocess.check_output("D=%s && printf \"${D#%s}\""%(path, get_fwroot()), shell=True, executable='/bin/bash')
