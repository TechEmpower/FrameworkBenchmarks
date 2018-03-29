import os
import socket
import fnmatch
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


def clean(benchmarker_config):
    '''
    Cleans all the docker images from the system
    '''
    # Clean the app server images
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)

    for image in client.images.list():
        # 'techempower/tfb.test.gemini:0.1' -> 'techempower/tfb.test.gemini'
        image_tag = image.tags[0].split(':')[0]
        if image_tag != 'techempower/tfb':
            client.images.remove(image.id, force=True)
    client.images.prune()

    # Clean the database server images
    client = docker.DockerClient(
        base_url=benchmarker_config.database_docker_host)

    for image in client.images.list():
        # 'techempower/tfb.test.gemini:0.1' -> 'techempower/tfb.test.gemini'
        image_tag = image.tags[0].split(':')[0]
        if image_tag != 'techempower/tfb':
            client.images.remove(image.id, force=True)
    client.images.prune()


def build(benchmarker_config, test_names, build_log_dir=os.devnull):
    '''
    Builds the dependency chain as well as the test implementation docker images
    for the given tests.
    '''
    tests = gather_tests(
        include=test_names, benchmarker_config=benchmarker_config)

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
            dependencies = OrderedSet(
                list(
                    reversed(
                        __gather_dependencies(
                            os.path.join(test.directory, test_docker_file)))))

            docker_dir = os.path.join(
                os.getenv('FWROOT'), "toolset", "setup", "docker")
            for dep in dependencies:
                pulled = False
                # Pull the dependency image
                try:
                    client = docker.DockerClient(
                        base_url=benchmarker_config.server_docker_host)
                    client.images.pull(dep)
                    pulled = True
                except:
                    pass

                if not pulled:
                    dep_ref = dep.strip().split(':')[0].strip()
                    dependency = dep_ref.split('/')[1]
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
                                    base_url=benchmarker_config.
                                    server_docker_host).build(
                                        path=os.path.dirname(docker_file),
                                        dockerfile="%s.dockerfile" %
                                        dependency,
                                        tag=dep,
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
                            base_url=benchmarker_config.server_docker_host
                    ).build(
                            path=test.directory,
                            dockerfile=test_docker_file,
                            tag="techempower/tfb.test.%s" %
                            test_docker_file.replace(".dockerfile", ""),
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
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)
    containers = []

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

            extra_hosts = None
            name = "tfb-server"

            if benchmarker_config.network is None:
                extra_hosts = {
                    socket.gethostname(): str(benchmarker_config.server_host),
                    'TFB-SERVER': str(benchmarker_config.server_host),
                    'TFB-DATABASE': str(benchmarker_config.database_host)
                }
                name = None

            sysctl = {'net.core.somaxconn': 65535}

            ulimit = [{
                'name': 'nofile',
                'hard': 200000,
                'soft': 200000
            }, {
                'name': 'rtprio',
                'hard': 99,
                'soft': 99
            }]

            container = client.containers.run(
                "techempower/tfb.test.%s" % docker_file.replace(
                    ".dockerfile", ""),
                name=name,
                network=benchmarker_config.network,
                network_mode=benchmarker_config.network_mode,
                stderr=True,
                detach=True,
                init=True,
                extra_hosts=extra_hosts,
                privileged=True,
                ulimits=ulimit,
                sysctls=sysctl)

            containers.append(container)

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

    return containers


def successfully_running_containers(benchmarker_config, docker_files, out):
    '''
    Returns whether all the expected containers for the given docker_files are
    running.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)
    expected_running_container_images = []
    for docker_file in docker_files:
        # 'gemini.dockerfile' -> 'gemini'
        image_tag = '.'.join(docker_file.split('.')[:-1])
        expected_running_container_images.append(image_tag)
    running_container_images = []
    for container in client.containers.list():
        # 'techempower/tfb.test.gemini:0.1' -> 'gemini'
        image_tag = container.image.tags[0].split(':')[0][21:]
        running_container_images.append(image_tag)

    for image_name in expected_running_container_images:
        if image_name not in running_container_images:
            log_prefix = "%s: " % image_name
            log("ERROR: Expected techempower/tfb.test.%s to be running container"
                % image_name,
                prefix=log_prefix,
                file=out)
            return False
    return True


def stop(benchmarker_config=None,
         containers=None,
         database_container=None,
         test=None):
    '''
    Attempts to stop the running test container.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)
    # Stop all our running containers
    for container in containers:
        container.stop()
        # 'techempower/tfb.test.gemini:0.1' -> 'techempower/tfb.test.gemini'
        client.images.remove(container.image.tags[0].split(':')[0], force=True)
    # Stop the database container
    if database_container is not None:
        database_container.stop()
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


def start_database(benchmarker_config, test, database):
    '''
    Sets up a container for the given database and port, and starts said docker 
    container.
    '''
    log_prefix = "%s: " % test.name

    database_dir = os.path.join(benchmarker_config.fwroot, "toolset", "setup",
                                "docker", "databases", database)
    docker_file = "%s.dockerfile" % database

    pulled = False
    # Pull the dependency image
    try:
        client = docker.DockerClient(
            base_url=benchmarker_config.database_docker_host)
        client.images.pull("techempower/%s" % database)
        pulled = True
    except:
        pass

    if not pulled:
        for line in docker.APIClient(
                base_url=benchmarker_config.database_docker_host).build(
                    path=database_dir,
                    dockerfile=docker_file,
                    tag="techempower/%s" % database):
            if line.startswith('{"stream":'):
                line = json.loads(line)
                line = line[line.keys()[0]].encode('utf-8')
                log(line,
                    prefix=log_prefix,
                    color=Fore.WHITE + Style.BRIGHT \
                        if re.match(r'^Step \d+\/\d+', line) else '')

    client = docker.DockerClient(
        base_url=benchmarker_config.database_docker_host)

    sysctl = {'net.core.somaxconn': 65535, 'kernel.sem': "250 32000 256 512"}

    ulimit = [{'name': 'nofile', 'hard': 65535, 'soft': 65535}]

    container = client.containers.run(
        "techempower/%s" % database,
        name="tfb-database",
        network=benchmarker_config.network,
        network_mode=benchmarker_config.network_mode,
        detach=True,
        ulimits=ulimit,
        sysctls=sysctl)

    # Sleep until the database accepts connections
    slept = 0
    max_sleep = 60
    while not test_database(benchmarker_config,
                            database) and slept < max_sleep:
        time.sleep(1)
        slept += 1

    return container


def test_client_connection(benchmarker_config, url):
    '''
    Tests that the app server at the given url responds successfully to a 
    request.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.client_docker_host)

    client.images.pull('techempower/tfb.wrk')

    try:
        client.containers.run(
            'techempower/tfb.wrk',
            'curl %s' % url,
            network=benchmarker_config.network,
            network_mode=benchmarker_config.network_mode)
    except:
        return False

    return True


def benchmark(benchmarker_config, script, variables, raw_file):
    '''
    Runs the given remote_script on the wrk container on the client machine.
    '''

    def watch_container(container, raw_file):
        with open(raw_file, 'w') as benchmark_file:
            for line in container.logs(stream=True):
                log(line, file=benchmark_file)

    client = docker.DockerClient(
        base_url=benchmarker_config.client_docker_host)

    sysctl = {'net.core.somaxconn': 65535}

    ulimit = [{'name': 'nofile', 'hard': 65535, 'soft': 65535}]

    watch_container(
        client.containers.run(
            "techempower/tfb.wrk",
            "/bin/bash /%s" % script,
            environment=variables,
            network=benchmarker_config.network,
            network_mode=benchmarker_config.network_mode,
            detach=True,
            stderr=True,
            ulimits=ulimit,
            sysctls=sysctl), raw_file)


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
                        deps.append(tokens[1])
                        dep_docker_file = os.path.join(
                            os.path.dirname(docker_file),
                            depToken + ".dockerfile")
                        if not os.path.exists(dep_docker_file):
                            dep_docker_file = find(docker_dir,
                                                   depToken + ".dockerfile")
                        deps.extend(__gather_dependencies(dep_docker_file))

    return deps