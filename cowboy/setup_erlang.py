
import subprocess
import sys
import setup_util

def start(args):
#  setup_util.replace_text("rails/config/database-ruby.yml", "host: .*", "host: " + args.database_host)
  
  try:
    subprocess.check_call("/usr/bin/curl -klO https://elearning.erlang-solutions.com/couchdb//rbingen_adapter//package_R16B_precise64_1361901944/esl-erlang_16.b-1~ubuntu~precise_amd64.deb", shell=True, cwd="cowboy")
    subprocess.check_call("sudo /usr/bin/dpkg --install esl-erlang_16.b-1~ubuntu~precise_amd64.deb", shell=True, cwd="cowboy")
    subprocess.check_call("./rebar get-deps", shell=True, cwd="cowboy")
    subprocess.check_call("./rebar compile", shell=True, cwd="cowboy")
    subprocess.check_call("erl -pa ebin deps/*/ebin -s hello_world -noshell -detached", shell=True, cwd="cowboy")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("killall beam", shell=True, cwd="/usr/bin")
    return 0
  except subprocess.CalledProcessError:
    return 1
