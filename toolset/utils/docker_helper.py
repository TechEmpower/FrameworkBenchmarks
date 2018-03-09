import os
import socket
import fnmatch
import subprocess
import multiprocessing
import json
import docker

from threading import Thread

from toolset.utils import setup_util
from toolset.utils.output_helper import tee_output
from toolset.utils.metadata_helper import gather_tests


def clean():
    '''
    Cleans all the docker images from the system
    '''
    subprocess.check_call(["docker", "image", "prune", "-f"])

    docker_ids = subprocess.check_output(["docker", "images",
                                          "-q"]).splitlines()
    for docker_id in docker_ids:
        subprocess.check_call(["docker", "image", "rmi", "-f", docker_id])

    subprocess.check_call(["docker", "system", "prune", "-a", "-f"])


def build(benchmarker_config, test_names, out):
    '''
    Builds the dependency chain as well as the test implementation docker images
    for the given tests.
    '''
    tests = gather_tests(test_names)

    for test in tests:
        docker_buildargs = {
            'CPU_COUNT': str(multiprocessing.cpu_count()),
            'MAX_CONCURRENCY': str(max(benchmarker_config.concurrency_levels)),
            'TFB_DATABASE': str(benchmarker_config.database_host)
        }

        test_docker_files = ["%s.dockerfile" % test.name]
        if test.docker_files is not None:
            if type(test.docker_files) is list:
                test_docker_files.extend(test.docker_files)
            else:
                raise Exception(
                    "docker_files in benchmark_config.json must be an array")

        for test_docker_file in test_docker_files:
            deps = list(
                reversed(
                    __gather_dependencies(
                        os.path.join(test.directory, test_docker_file))))

            docker_dir = os.path.join(setup_util.get_fwroot(), "toolset",
                                      "setup", "docker")
            for dependency in deps:
                docker_file = os.path.join(test.directory,
                                           dependency + ".dockerfile")
                if not docker_file or not os.path.exists(docker_file):
                    docker_file = find(docker_dir, dependency + ".dockerfile")
                if not docker_file:
                    tee_output(
                        out,
                        "Docker build failed; %s could not be found; terminating\n"
                        % (dependency + ".dockerfile"))
                    return 1

                # Build the dependency image
                try:
                    for line in docker.APIClient(
                            base_url='unix://var/run/docker.sock').build(
                                path=os.path.dirname(docker_file),
                                dockerfile="%s.dockerfile" % dependency,
                                tag="tfb/%s" % dependency,
                                buildargs=docker_buildargs,
                                forcerm=True):
                        prev_line = os.linesep
                        if line.startswith('{"stream":'):
                            line = json.loads(line)
                            line = line[line.keys()[0]].encode('utf-8')
                            if prev_line.endswith(os.linesep):
                                tee_output(out, line)
                            else:
                                tee_output(out, line)
                            prev_line = line
                except Exception as e:
                    tee_output(out,
                               "Docker dependency build failed; terminating\n")
                    print(e)
                    return 1

        # Build the test images
        for test_docker_file in test_docker_files:
            try:
                for line in docker.APIClient(
                        base_url='unix://var/run/docker.sock').build(
                            path=test.directory,
                            dockerfile=test_docker_file,
                            tag="tfb/test/%s" % test_docker_file.replace(
                                ".dockerfile", ""),
                            buildargs=docker_buildargs,
                            forcerm=True):
                    prev_line = os.linesep
                    if line.startswith('{"stream":'):
                        line = json.loads(line)
                        line = line[line.keys()[0]].encode('utf-8')
                        if prev_line.endswith(os.linesep):
                            tee_output(out, line)
                        else:
                            tee_output(out, line)
                        prev_line = line
            except Exception as e:
                tee_output(out, "Docker build failed; terminating\n")
                print(e)
                return 1

    return 0


def run(benchmarker_config, docker_files, out):
    '''
    Run the given Docker container(s)
    '''
    client = docker.from_env()

    for docker_file in docker_files:
        try:

            def watch_container(container):
                for line in container.logs(stream=True):
                    tee_output(out, line)

            extra_hosts = {
                socket.gethostname(): str(benchmarker_config.server_host),
                'TFB-SERVER': str(benchmarker_config.server_host),
                'TFB-DATABASE': str(benchmarker_config.database_host),
                'TFB-CLIENT': str(benchmarker_config.client_host)
            }

            container = client.containers.run(
                "tfb/test/%s" % docker_file.replace(".dockerfile", ""),
                network_mode="host",
                privileged=True,
                stderr=True,
                detach=True,
                init=True,
                extra_hosts=extra_hosts)

            watch_thread = Thread(target=watch_container, args=(container, ))
            watch_thread.daemon = True
            watch_thread.start()

        except Exception as e:
            tee_output(out,
                       "Running docker cointainer: %s failed" % docker_file)
            print(e)
            return 1

    running_container_length = len(
        client.containers.list(filters={'status': 'running'}))
    expected_length = len(docker_files)
    if (running_container_length < expected_length):
        tee_output(out, "Running Containers (id, name):" + os.linesep)
        for running_container in client.containers.list():
            tee_output(out, "%s, %s%s" % (running_container.short_id,
                                          running_container.image, os.linesep))
        tee_output(out, "Excepted %s running containers; saw %s%s" %
                   (running_container_length, expected_length, os.linesep))
        return 1

    return 0


def stop(config, database_container_id, test, out):
    '''
    Attempts to stop the running test container.
    '''
    client = docker.from_env()
    # Stop all the containers
    for container in client.containers.list():
        if container.status == "running" and container.id != database_container_id:
            container.stop()
    # Remove only the tfb/test image for this test
    try:
        client.images.remove("tfb/test/%s" % test.name, force=True)
    except:
        # This can be okay if the user hit ctrl+c before the image built/ran
        pass
    # Stop the database container
    if database_container_id:
        p = subprocess.Popen(
            config.database_ssh_string,
            stdin=subprocess.PIPE,
            shell=True,
            stdout=config.quiet_out,
            stderr=subprocess.STDOUT)
        p.communicate("docker stop %s" % database_container_id)
    client.images.prune()


def find(path, pattern):
    '''
    Finds and returns all the the files matching the given pattern recursively in
    the given path. 
    '''
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                return os.path.join(root, name)


def start_database(config, database):
    '''
    Sets up a container for the given database and port, and starts said docker 
    container.
    '''

    def __is_hex(s):
        try:
            int(s, 16)
        except ValueError:
            return False
        return len(s) % 2 == 0

    p = subprocess.Popen(
        config.database_ssh_string,
        stdin=subprocess.PIPE,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT)
    out = p.communicate("docker images  -q %s" % database)[0]
    dbid = ''
    if len(out.splitlines()) > 0:
        dbid = out.splitlines()[len(out.splitlines()) - 1]

    # If the database image exists, then dbid will look like
    # fe12ca519b47, and we do not want to rebuild if it exists
    if len(dbid) != 12 and not __is_hex(dbid):

        def __scp_string(files):
            scpstr = ["scp", "-i", config.database_identity_file]
            for file in files:
                scpstr.append(file)
            scpstr.append("%s@%s:~/%s/" % (config.database_user,
                                           config.database_host, database))
            return scpstr

        p = subprocess.Popen(
            config.database_ssh_string,
            shell=True,
            stdin=subprocess.PIPE,
            stdout=config.quiet_out,
            stderr=subprocess.STDOUT)
        p.communicate("mkdir -p %s" % database)
        dbpath = os.path.join(config.fwroot, "toolset", "setup", "docker",
                              "databases", database)
        dbfiles = ""
        for dbfile in os.listdir(dbpath):
            dbfiles += "%s " % os.path.join(dbpath, dbfile)
        p = subprocess.Popen(
            __scp_string(dbfiles.split()),
            stdin=subprocess.PIPE,
            stdout=config.quiet_out,
            stderr=subprocess.STDOUT)
        p.communicate()
        p = subprocess.Popen(
            config.database_ssh_string,
            shell=True,
            stdin=subprocess.PIPE,
            stdout=config.quiet_out,
            stderr=subprocess.STDOUT)
        p.communicate("docker build -f ~/%s/%s.dockerfile -t %s ~/%s" %
                      (database, database, database, database))
        if p.returncode != 0:
            return None

    p = subprocess.Popen(
        config.database_ssh_string,
        stdin=subprocess.PIPE,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT)
    out = p.communicate("docker run -d --rm --network=host %s" % database)[0]
    return out.splitlines()[len(out.splitlines()) - 1]


def __gather_dependencies(docker_file):
    '''
    Gathers all the known docker dependencies for the given docker image.
    '''
    # Avoid setting up a circular import
    from toolset.utils import setup_util
    deps = []

    docker_dir = os.path.join(setup_util.get_fwroot(), "toolset", "setup",
                              "docker")

    if os.path.exists(docker_file):
        with open(docker_file) as fp:
            for line in fp.readlines():
                tokens = line.strip().split(' ')
                if tokens[0] == "FROM":
                    # This is magic that our base image points to
                    if tokens[1] != "ubuntu:16.04":
                        dep_ref = tokens[1].strip().split(':')[0].strip()
                        if '/' not in dep_ref:
                            raise AttributeError(
                                "Could not find docker FROM dependency: %s" %
                                dep_ref)
                        depToken = dep_ref.split('/')[1]
                        deps.append(depToken)
                        dep_docker_file = os.path.join(
                            os.path.dirname(docker_file),
                            depToken + ".dockerfile")
                        if not os.path.exists(dep_docker_file):
                            dep_docker_file = find(docker_dir,
                                                   depToken + ".dockerfile")
                        deps.extend(__gather_dependencies(dep_docker_file))

    return deps