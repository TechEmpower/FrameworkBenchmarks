import os
import socket
import fnmatch
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

    client.images.prune()
    for image in client.images.list():
        if len(image.tags) > 0:
            # 'techempower/tfb.test.gemini:0.1' -> 'techempower/tfb.test.gemini'
            image_tag = image.tags[0].split(':')[0]
            if image_tag != 'techempower/tfb' and 'techempower' in image_tag:
                client.images.remove(image.id, force=True)
    client.images.prune()

    # Clean the database server images
    client = docker.DockerClient(
        base_url=benchmarker_config.database_docker_host)

    client.images.prune()
    for image in client.images.list():
        if len(image.tags) > 0:
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

        # Build the test image
        test_docker_file = "%s.dockerfile" % test.name
        build_log_file = build_log_dir
        if build_log_dir is not os.devnull:
            build_log_file = os.path.join(
                build_log_dir,
                "%s.log" % test_docker_file.replace(".dockerfile", "").lower())
        with open(build_log_file, 'w') as build_log:
            try:
                for line in docker.APIClient(
                        base_url=benchmarker_config.server_docker_host).build(
                            path=test.directory,
                            dockerfile=test_docker_file,
                            tag="techempower/tfb.test.%s" %
                            test_docker_file.replace(".dockerfile", ""),
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


def run(benchmarker_config, test, run_log_dir):
    '''
    Run the given Docker container(s)
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)
    containers = []

    log_prefix = "%s: " % test.name
    try:

        def watch_container(container, docker_file):
            with open(
                    os.path.join(run_log_dir, "%s.log" % docker_file.replace(
                        ".dockerfile", "").lower()), 'w') as run_log:
                for line in container.logs(stream=True):
                    log(line, prefix=log_prefix, file=run_log)

        extra_hosts = None
        name = "tfb-server"

        if benchmarker_config.network is None:
            extra_hosts = {
                socket.gethostname(): str(benchmarker_config.server_host),
                'tfb-server': str(benchmarker_config.server_host),
                'tfb-database': str(benchmarker_config.database_host)
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
            "techempower/tfb.test.%s" % test.name,
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
            target=watch_container,
            args=(
                container,
                "%s.dockerfile" % test.name,
            ))
        watch_thread.daemon = True
        watch_thread.start()

    except Exception:
        with open(
                os.path.join(run_log_dir, "%s.log" % test.name.lower()),
                'w') as run_log:
            tb = traceback.format_exc()
            log("Running docker cointainer: %s.dockerfile failed" % test.name,
                prefix=log_prefix,
                file=run_log)
            log(tb, prefix=log_prefix, file=run_log)

    return containers


def successfully_running_containers(benchmarker_config, test, out):
    '''
    Returns whether all the expected containers for the given docker_files are
    running.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)
    running_container_images = []
    for container in client.containers.list():
        # 'techempower/tfb.test.gemini:0.1' -> 'gemini'
        image_tag = container.image.tags[0].split(':')[0][21:]
        running_container_images.append(image_tag)

    if test.name not in running_container_images:
        log_prefix = "%s: " % test.name
        log("ERROR: Expected techempower/tfb.test.%s to be running container" %
            test.name,
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
    if containers is None:
        for container in client.containers.list():
            if len(
                    container.image.tags
            ) > 0 and 'techempower' in container.image.tags[0] and 'tfb:latest' not in container.image.tags[0]:
                container.stop()
    else:
        # Stop all our running containers
        for container in containers:
            container.stop()

    database_client = docker.DockerClient(
        base_url=benchmarker_config.database_docker_host)
    # Stop the database container
    if database_container is None:
        for container in database_client.containers.list():
            if len(
                    container.image.tags
            ) > 0 and 'techempower' in container.image.tags[0] and 'tfb:latest' not in container.image.tags[0]:
                container.stop()
    else:
        database_container.stop()

    client.containers.prune()

    if benchmarker_config.server_docker_host != benchmarker_config.database_docker_host:
        database_client.containers.prune()


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
    image_name = "techempower/%s:latest" % database
    log_prefix = image_name + ": "

    database_dir = os.path.join(benchmarker_config.fwroot, "toolset",
                                "databases", database)
    docker_file = "%s.dockerfile" % database

    pulled = False
    client = docker.DockerClient(
        base_url=benchmarker_config.database_docker_host)
    try:
        # Don't pull if we have it
        client.images.get(image_name)
        pulled = True
        log("Found published image; skipping build", prefix=log_prefix)
    except:
        # Pull the dependency image
        try:
            log("Attempting docker pull for image (this can take some time)",
                prefix=log_prefix)
            client.images.pull(image_name)
            pulled = True
            log("Found published image; skipping build", prefix=log_prefix)
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
    database_ready = False
    while not database_ready and slept < max_sleep:
        time.sleep(1)
        slept += 1
        database_ready = test_database(benchmarker_config, database)

    if not database_ready:
        log("Database was not ready after startup", prefix=log_prefix)

    return container


def test_client_connection(benchmarker_config, url):
    '''
    Tests that the app server at the given url responds successfully to a 
    request.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.client_docker_host)

    try:
        client.images.get('techempower/tfb.wrk:latest')
    except:
        log("Attempting docker pull for image (this can take some time)",
            prefix="techempower/tfb.wrk:latest: ")
        client.images.pull('techempower/tfb.wrk:latest')

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
