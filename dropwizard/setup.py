import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args):
    setup_util.replace_text("dropwizard/hello-world.yml", "url: jdbc:mysql://localhost/hello_world", "url: jdbc:mysql://" + args.database_host + ":3306/hello_world")

    try:
        subprocess.check_call("mvn package;", shell=True, cwd="dropwizard")
        subprocess.check_call("java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world.yml", shell=True, cwd="dropwizard")
        return 0
    except subprocess.CalledProcessError:
        return 1
def stop():
    try:
        subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True)
        return 0
    except subprocess.CalledProcessError:
        return 1

