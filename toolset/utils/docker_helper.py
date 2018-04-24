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


def __build(base_url, path, build_log_file, log_prefix, dockerfile, tag):
    '''
    Builds docker containers using docker-py low-level api
    '''

    with open(build_log_file, 'w') as build_log:
        try:
            client = docker.APIClient(base_url=base_url)
            output = client.build(
                path=path,
                dockerfile=dockerfile,
                tag=tag,
                forcerm=True,
                pull=True)
            buffer = ""
            for token in output:
                if token.startswith('{"stream":'):
                    token = json.loads(token)
                    token = token[token.keys()[0]].encode('utf-8')
                    buffer += token
                elif token.startswith('{"errorDetail":'):
                    token = json.loads(token)
                    raise Exception(token['errorDetail']['message'])
                while "\n" in buffer:
                    index = buffer.index("\n")
                    line = buffer[:index]
                    buffer = buffer[index + 1:]
                    log(line,
                        prefix=log_prefix,
                        file=build_log,
                        color=Fore.WHITE + Style.BRIGHT \
                            if re.match(r'^Step \d+\/\d+', line) else '')

            if buffer:
                log(buffer,
                    prefix=log_prefix,
                    file=build_log,
                    color=Fore.WHITE + Style.BRIGHT \
                        if re.match(r'^Step \d+\/\d+', buffer) else '')
        except Exception:
            tb = traceback.format_exc()
            log("Docker build failed; terminating",
                prefix=log_prefix,
                file=build_log,
                color=Fore.RED)
            log(tb, prefix=log_prefix, file=build_log)
            raise


def build(benchmarker_config, test_names, build_log_dir=os.devnull):
    '''
    Builds the test docker containers
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

        try:
            __build(
                base_url=benchmarker_config.server_docker_host,
                build_log_file=build_log_file,
                log_prefix=log_prefix,
                path=test.directory,
                dockerfile=test_docker_file,
                tag="techempower/tfb.test.%s" % test_docker_file.replace(
                    ".dockerfile", ""))
        except Exception:
            return 1

    return 0


def run(benchmarker_config, test, run_log_dir):
    '''
    Run the given Docker container(s)
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)

    log_prefix = "%s: " % test.name
    container = None

    try:

        def watch_container(docker_container, docker_file):
            with open(
                    os.path.join(run_log_dir, "%s.log" % docker_file.replace(
                        ".dockerfile", "").lower()), 'w') as run_log:
                for line in docker_container.logs(stream=True):
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
            sysctls=sysctl,
            remove=True,
            log_config={'type': None})

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

    return container


def stop(benchmarker_config=None,
         container=None,
         database_container=None,
         test=None):
    '''
    Attempts to stop the running test container.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)
    if container is None:
        for container in client.containers.list():
            if len(
                    container.image.tags
            ) > 0 and 'techempower' in container.image.tags[0] and 'tfb:latest' not in container.image.tags[0]:
                container.stop()
    else:
        # Stop the running container
        try:
            container.stop()
        except Exception:
            # Suppress "No such container" errors
            pass

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
        try:
            database_container.stop()
        except Exception:
            # Suppress "No such container" errors
            pass

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


def start_database(benchmarker_config, database):
    '''
    Sets up a container for the given database and port, and starts said docker
    container.
    '''
    image_name = "techempower/%s:latest" % database
    log_prefix = image_name + ": "

    database_dir = os.path.join(benchmarker_config.fwroot, "toolset",
                                "databases", database)
    docker_file = "%s.dockerfile" % database

    __build(
        base_url=benchmarker_config.database_docker_host,
        path=database_dir,
        dockerfile=docker_file,
        log_prefix=log_prefix,
        build_log_file=os.devnull,
        tag="techempower/%s" % database)

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
        sysctls=sysctl,
        remove=True,
        log_config={'type': None})

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


def build_wrk(benchmarker_config):
    '''
    Builds the techempower/tfb.wrk container
    '''
    __build(
        base_url=benchmarker_config.client_docker_host,
        path=os.path.join(benchmarker_config.fwroot, "toolset", "wrk"),
        dockerfile="wrk.dockerfile",
        log_prefix="wrk: ",
        build_log_file=os.devnull,
        tag="techempower/tfb.wrk")


def test_client_connection(benchmarker_config, url):
    '''
    Tests that the app server at the given url responds successfully to a
    request.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.client_docker_host)

    try:
        client.containers.run(
            'techempower/tfb.wrk',
            'curl %s' % url,
            remove=True,
            log_config={'type': None},
            network=benchmarker_config.network,
            network_mode=benchmarker_config.network_mode)
    except:
        return False

    return True


def server_container_exists(benchmarker_config, container_id_or_name):
    '''
    Returns True if the container still exists on the server.
    '''
    client = docker.DockerClient(
        base_url=benchmarker_config.server_docker_host)
    try:
        client.containers.get(container_id_or_name)
        return True
    except:
        return False


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
            "/bin/bash %s" % script,
            environment=variables,
            network=benchmarker_config.network,
            network_mode=benchmarker_config.network_mode,
            detach=True,
            stderr=True,
            ulimits=ulimit,
            sysctls=sysctl,
            remove=True,
            log_config={'type': None}), raw_file)
