import subprocess

def start(args):
  setup_util.replace_text("kelp/app.pl", "localhost", args.database_host)
  subprocess.Popen("plackup -E deployment -s Starman -p 8080 kelp/app.pl".rsplit(" "))
  return 0
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'plackup' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
