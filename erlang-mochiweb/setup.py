import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
    setup_util.replace_text("erlang-mochiweb/priv/app.config",
                            "{db_host, \".*\"}",
                            "{db_host, \"" + args.database_host + "\"}")

    try:
        subprocess.check_call("make clean", shell=True, cwd="mochiweb", stderr=errfile, stdout=logfile)
        subprocess.check_call("make", shell=True, cwd="mochiweb", stderr=errfile, stdout=logfile)
        subprocess.check_call("make run", shell=True, cwd="mochiweb", stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    try:
        subprocess.check_call("make stop", shell=True, cwd="/usr/bin")
        return 0
    except subprocess.CalledProcessError:
        return 1
