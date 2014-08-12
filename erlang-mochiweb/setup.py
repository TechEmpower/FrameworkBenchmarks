import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
    setup_util.replace_text("erlang-mochiweb/priv/app.config",
                            "{db_host, \".*\"}",
                            "{db_host, \"" + args.database_host + "\"}")

    try:
        cmd = "erl +K true +sbwt very_long +swt very_low -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -s mochiweb_bench_app"
        subprocess.check_call("./rebar clean get-deps compile", shell=True, cwd="erlang-mochiweb", stderr=errfile, stdout=logfile)
        subprocess.check_call(cmd, shell=True, cwd="erlang-mochiweb", stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    try:
        subprocess.check_call("killall beam.smp", shell=True, cwd="/usr/bin")
        return 0
    except subprocess.CalledProcessError:
        return 1
