import os
import socket
import fnmatch
import subprocess
import multiprocessing
import json
import docker
import time
import re
import traceback
from threading import Thread
from colorama import Fore, Style

from toolset.utils.output_helper import log
from toolset.utils.metadata_helper import gather_tests
from toolset.utils.ordered_set import OrderedSet
from toolset.utils.database_helper import test_database


def clean(config):
    '''
    Cleans all the docker images from the system
    '''
    # Clean the app server images
    # subprocess.check_call(["docker", "image", "prune", "-f"])

    # docker_ids = subprocess.check_output(["docker", "images",
    #                                       "-q"]).splitlines()
    # for docker_id in docker_ids:
    #     subprocess.check_call(["docker", "image", "rmi", "-f", docker_id])

    # subprocess.check_call(["docker", "system", "prune", "-a", "-f"])

    # Clean the database server images
    # command = list(config.database_ssh_command)
    # command.extend(["docker", "image", "prune", "-f"])
    # subprocess.check_call(command)

    # command = list(config.database_ssh_command)
    # command.extend(["docker", "images", "-q"])
    # docker_ids = subprocess.check_output(command).splitlines()

    # for docker_id in docker_ids:
    #     command = list(config.database_ssh_command)
    #     command.extend(["docker", "image", "rmi", "-f", docker_id])
    #     subprocess.check_call(command)

    # command = list(config.database_ssh_command)
    # command.extend(["docker", "system", "prune", "-a", "-f"])
    # subprocess.check_call(command)

    client = docker.from_env()

    for image in client.images.list():
        client.images.remove(image.id)
    client.images.prune()


def build(benchmarker_config, test_names, build_log_dir=os.devnull):
    '''
    Builds the dependency chain as well as the test implementation docker images
    for the given tests.
    '''
    tests = gather_tests(test_names)

    for test in tests:
        log_prefix = "%s: " % test.name

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
            deps = OrderedSet(
                list(
                    reversed(
                        __gather_dependencies(
                            os.path.join(test.directory, test_docker_file)))))

            docker_dir = os.path.join(
                os.getenv('FWROOT'), "toolset", "setup", "docker")
            for dependency in deps:
                build_log_file = build_log_dir
                if build_log_dir is not os.devnull:
                    build_log_file = os.path.join(
                        build_log_dir, "%s.log" % dependency.lower())
                with open(build_log_file, 'w') as build_log:
                    docker_file = os.path.join(test.directory,
                                               dependency + ".dockerfile")
                    if not docker_file or not os.path.exists(docker_file):
                        docker_file = find(docker_dir,
                                           dependency + ".dockerfile")
                    if not docker_file:
                        log("Docker build failed; %s could not be found; terminating"
                            % (dependency + ".dockerfile"),
                            prefix=log_prefix,
                            file=build_log,
                            color=Fore.RED)
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
                            if line.startswith('{"stream":'):
                                line = json.loads(line)
                                line = line[line.keys()[0]].encode('utf-8')
                                log(line,
                                    prefix=log_prefix,
                                    file=build_log,
                                    color=Fore.WHITE + Style.BRIGHT \
                                        if re.match(r'^Step \d+\/\d+', line) else '')
                    except Exception:
                        tb = traceback.format_exc()
                        log("Docker dependency build failed; terminating",
                            prefix=log_prefix,
                            file=build_log,
                            color=Fore.RED)
                        log(tb, prefix=log_prefix, file=build_log)
                        return 1

        # Build the test images
        for test_docker_file in test_docker_files:
            build_log_file = build_log_dir
            if build_log_dir is not os.devnull:
                build_log_file = os.path.join(
                    build_log_dir, "%s.log" % test_docker_file.replace(
                        ".dockerfile", "").lower())
            with open(build_log_file, 'w') as build_log:
                try:
                    for line in docker.APIClient(
                            base_url='unix://var/run/docker.sock').build(
                                path=test.directory,
                                dockerfile=test_docker_file,
                                tag="tfb/test/%s" % test_docker_file.replace(
                                    ".dockerfile", ""),
                                buildargs=docker_buildargs,
                                forcerm=True):
                        if line.startswith('{"stream":'):
                            line = json.loads(line)
                            line = line[line.keys()[0]].encode('utf-8')
                            log(line,
                                prefix=log_prefix,
                                file=build_log,
                                color=Fore.WHITE + Style.BRIGHT \
                                    if re.match(r'^Step \d+\/\d+', line) else '')
                except Exception:
                    tb = traceback.format_exc()
                    log("Docker build failed; terminating",
                        prefix=log_prefix,
                        file=build_log,
                        color=Fore.RED)
                    log(tb, prefix=log_prefix, file=build_log)
                    return 1

    return 0


def run(benchmarker_config, docker_files, run_log_dir):
    '''
    Run the given Docker container(s)
    '''
    client = docker.from_env()

    for docker_file in docker_files:
        log_prefix = "%s: " % docker_file.replace(".dockerfile", "")
        try:

            def watch_container(container, docker_file):
                with open(
                        os.path.join(
                            run_log_dir, "%s.log" % docker_file.replace(
                                ".dockerfile", "").lower()), 'w') as run_log:
                    for line in container.logs(stream=True):
                        log(line, prefix=log_prefix, file=run_log)

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

            watch_thread = Thread(
                target=watch_container, args=(
                    container,
                    docker_file,
                ))
            watch_thread.daemon = True
            watch_thread.start()

        except Exception:
            with open(
                    os.path.join(run_log_dir, "%s.log" % docker_file.replace(
                        ".dockerfile", "").lower()), 'w') as run_log:
                tb = traceback.format_exc()
                log("Running docker cointainer: %s failed" % docker_file,
                    prefix=log_prefix,
                    file=run_log)
                log(tb, prefix=log_prefix, file=run_log)
                return 1

    return 0


def successfully_running_containers(docker_files, out):
    '''
    Returns whether all the expected containers for the given docker_files are
    running.
    '''
    client = docker.from_env()
    expected_running_container_images = []
    for docker_file in docker_files:
        # 'gemini.dockerfile' -> 'gemini'
        image_tag = docker_file.split('.')[0]
        expected_running_container_images.append(image_tag)
    running_container_images = []
    for container in client.containers.list():
        # 'tfb/test/gemini:latest' -> 'gemini'
        image_tag = container.image.tags[0].split(':')[0][9:]
        running_container_images.append(image_tag)

    for image_name in expected_running_container_images:
        if image_name not in running_container_images:
            log_prefix = "%s: " % image_name
            log("ERROR: Expected tfb/test/%s to be running container" %
                image_name,
                prefix=log_prefix,
                file=out)
            return False
    return True


def stop(config=None, database_container_id=None, test=None):
    '''
    Attempts to stop the running test container.
    '''
    client = docker.from_env()
    # Stop all the containers
    for container in client.containers.list():
        image_name = container.image.tags[0].split(':')[0]
        if container.status == "running" and container.id != database_container_id and image_name != 'tfb':
            container.stop()
    # Remove only the tfb/test image for this test
    try:
        client.images.remove("tfb/test/%s" % test.name, force=True)
    except:
        # This can be okay if the user hit ctrl+c before the image built/ran
        pass
    # Stop the database container
    if database_container_id:
        client = docker.from_env()
        client.containers.get(database_container_id).stop()
    client.images.prune()
    client.containers.prune()
    client.networks.prune()
    client.volumes.prune()


def find(path, pattern):
    '''
    Finds and returns all the the files matching the given pattern recursively in
    the given path. 
    '''
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                return os.path.join(root, name)


def start_database(config, test, database):
    '''
    Sets up a container for the given database and port, and starts said docker 
    container.
    '''
    log_prefix = "%s: " % test.name

    database_dir = os.path.join(config.fwroot, "toolset", "setup", "docker",
                                "databases", database)
    docker_file = "%s.dockerfile" % database

    for line in docker.APIClient(base_url='unix://var/run/docker.sock').build(
            path=database_dir, dockerfile=docker_file,
            tag="tfb/%s" % database):
        if line.startswith('{"stream":'):
            line = json.loads(line)
            line = line[line.keys()[0]].encode('utf-8')
            log(line,
                prefix=log_prefix,
                color=Fore.WHITE + Style.BRIGHT \
                    if re.match(r'^Step \d+\/\d+', line) else '')

    client = docker.from_env()

    container = client.containers.run(
        "tfb/%s" % database, network_mode="host", privileged=True, detach=True)

    # Sleep until the database accepts connections
    slept = 0
    max_sleep = 60
    while not test_database(config, database) and slept < max_sleep:
        time.sleep(1)
        slept += 1

    return container.id


def test_client_connection(url):
    '''
    Tests that the app server at the given url responds successfully to a 
    request.
    '''
    client = docker.from_env()

    try:
        client.containers.run('wrk', 'curl %s' % url, network_mode="host")
    except:
        return False

    return True


def __gather_dependencies(docker_file):
    '''
    Gathers all the known docker dependencies for the given docker image.
    '''
    deps = []

    docker_dir = os.path.join(
        os.getenv('FWROOT'), "toolset", "setup", "docker")

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