
import subprocess
import sys
import setup_util

def start(args):
  setup_util.replace_text("restexpress/config/dev/environment.properties", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

  try:
    subprocess.check_call("mvn clean package", shell=True, cwd="restexpress")
    subprocess.check_call("mvn assembly:single", shell=True, cwd="restexpress")
    subprocess.check_call("unzip world-1.0-SNAPSHOT-zip-with-dependencies.zip", shell=True, cwd="restexpress/target")
    subprocess.Popen("java -jar world-1.0-SNAPSHOT.jar".rsplit(" "), cwd="restexpress/target/world-1.0-SNAPSHOT")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello.Main' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0