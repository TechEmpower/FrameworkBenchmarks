import subprocess
import os
import setup_util

def start(args, logfile, errfile, cwd='wt'):
    setup_util.replace_text("wt/benchmark.cpp", "INSERT_DB_HOST_HERE", args.database_host);
    subprocess.check_call('g++ -O3 -DNDEBUG -DBENCHMARK_USE_POSTGRES -std=c++0x -L/usr/local/lib -I/usr/local/include -o benchmark_postgres.wt benchmark.cpp -lwt -lwthttp -lwtdbo -lwtdbopostgres -lboost_thread', shell=True, cwd=cwd, stderr=errfile, stdout=logfile)
    os.environ['LD_LIBRARY_PATH'] = '/usr/local/lib:' + os.environ.get('LD_LIBRARY_PATH', '.')
    subprocess.Popen(['./benchmark_postgres.wt',
        '-c', 'wt_config.xml',
        '-t', str(args.max_threads * 4),
    	'--docroot', '.',
        '--http-address', '0.0.0.0',
        '--http-port', '8080',
        '--accesslog=-',
        '--no-compression'],
        cwd=cwd, stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
        if 'benchmark_postgres.wt' in line:
            pid = int(line.split(None, 2)[1])
            os.kill(pid, 15)
    return 0
