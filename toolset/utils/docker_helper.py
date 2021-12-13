import os
import socket
import json
import docker
import time
import re
import traceback
from threading import Thread
from colorama import Fore, Style

from toolset.utils.output_helper import log
from toolset.databases import databases

from psutil import virtual_memory

# total memory limit allocated for the test container
mem_limit = int(round(virtual_memory().total * .95))

class DockerHelper:
    def __init__(self, benchmarker=None):
        self.benchmarker = benchmarker

        self.client = docker.DockerClient(
            base_url=self.benchmarker.config.client_docker_host)
        self.server = docker.DockerClient(
            base_url=self.benchmarker.config.server_docker_host)
        self.database = docker.DockerClient(
            base_url=self.benchmarker.config.database_docker_host)

    def __build(self, base_url, path, build_log_file, log_prefix, dockerfile,
                tag, buildargs={}):
        '''
        Builds docker containers using docker-py low-level api
        '''

        self.benchmarker.time_logger.mark_build_start()
        with open(build_log_file, 'w') as build_log:
            try:
                client = docker.APIClient(base_url=base_url)
                output = client.build(
                    path=path,
                    dockerfile=dockerfile,
                    tag=tag,
                    forcerm=True,
                    timeout=3600,
                    pull=True,
                    buildargs=buildargs,
                    decode=True
                )
                buffer = ""
                for token in output:
                    if 'stream' in token:
                        buffer += token[token.keys()[0]].encode('utf-8')
                    elif 'errorDetail' in token:
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
                    # Kill docker builds if they exceed 60 mins. This will only
                    # catch builds that are still printing output.
                    if self.benchmarker.time_logger.time_since_start() > 3600:
                        log("Build time exceeded 60 minutes",
                            prefix=log_prefix,
                            file=build_log,
                            color=Fore.RED)
                        raise Exception

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
                self.benchmarker.time_logger.log_build_end(
                    log_prefix=log_prefix, file=build_log)
                raise

            self.benchmarker.time_logger.log_build_end(
                log_prefix=log_prefix, file=build_log)

    def clean(self):
        '''
        Cleans all the docker images from the system
        '''

        self.server.images.prune()
        for image in self.server.images.list():
            if len(image.tags) > 0:
                # 'techempower/tfb.test.gemini:0.1' -> 'techempower/tfb.test.gemini'
                image_tag = image.tags[0].split(':')[0]
                if image_tag != 'techempower/tfb' and 'techempower' in image_tag:
                    self.server.images.remove(image.id, force=True)
        self.server.images.prune()

        self.database.images.prune()
        for image in self.database.images.list():
            if len(image.tags) > 0:
                # 'techempower/tfb.test.gemini:0.1' -> 'techempower/tfb.test.gemini'
                image_tag = image.tags[0].split(':')[0]
                if image_tag != 'techempower/tfb' and 'techempower' in image_tag:
                    self.database.images.remove(image.id, force=True)
        self.database.images.prune()

    def build(self, test, build_log_dir=os.devnull):
        '''
        Builds the test docker containers
        '''
        log_prefix = "%s: " % test.name

        # Build the test image
        test_docker_file = '%s.dockerfile' % test.name
        if hasattr(test, 'dockerfile'):
            test_docker_file = test.dockerfile
        test_database = ''
        if hasattr(test, 'database'):
            test_database = test.database
        build_log_file = build_log_dir
        if build_log_dir is not os.devnull:
            build_log_file = os.path.join(
                build_log_dir,
                "%s.log" % test_docker_file.replace(".dockerfile", "").lower())

        try:
            self.__build(
                base_url=self.benchmarker.config.server_docker_host,
                build_log_file=build_log_file,
                log_prefix=log_prefix,
                path=test.directory,
                dockerfile=test_docker_file,
                buildargs=({
                    'BENCHMARK_ENV':
                        self.benchmarker.config.results_environment,
                    'TFB_TEST_NAME': test.name,
                    'TFB_TEST_DATABASE': test_database
                }),
                tag="techempower/tfb.test.%s" % test.name)
        except Exception:
            return 1

        return 0

    def run(self, test, run_log_dir):
        '''
        Run the given Docker container(s)
        '''

        log_prefix = "%s: " % test.name
        container = None

        try:

            def watch_container(docker_container, docker_file):
                with open(
                        os.path.join(
                            run_log_dir, "%s.log" % docker_file.replace(
                                ".dockerfile", "").lower()), 'w') as run_log:
                    for line in docker_container.logs(stream=True):
                        log(line, prefix=log_prefix, file=run_log)

            extra_hosts = None
            name = "tfb-server"

            if self.benchmarker.config.network is None:
                extra_hosts = {
                    socket.gethostname():
                    str(self.benchmarker.config.server_host),
                    'tfb-server':
                    str(self.benchmarker.config.server_host),
                    'tfb-database':
                    str(self.benchmarker.config.database_host)
                }
                name = None

            if self.benchmarker.config.network_mode is None:
                sysctl = {'net.core.somaxconn': 65535}
            else:
                # Do not pass `net.*` kernel params when using host network mode
                sysctl = None

            ulimit = [{
                'name': 'nofile',
                'hard': 200000,
                'soft': 200000
            }, {
                'name': 'rtprio',
                'hard': 99,
                'soft': 99
            }]

            docker_cmd = ''
            if hasattr(test, 'docker_cmd'):
                docker_cmd = test.docker_cmd

            # Expose ports in debugging mode
            ports = {}
            if self.benchmarker.config.mode == "debug":
                ports = {test.port: test.port}

            container = self.server.containers.run(
                "techempower/tfb.test.%s" % test.name,
                name=name,
                command=docker_cmd,
                network=self.benchmarker.config.network,
                network_mode=self.benchmarker.config.network_mode,
                ports=ports,
                stderr=True,
                detach=True,
                init=True,
                extra_hosts=extra_hosts,
                privileged=True,
                ulimits=ulimit,
                mem_limit=mem_limit,
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
                log("Running docker container: %s.dockerfile failed" %
                    test.name,
                    prefix=log_prefix,
                    file=run_log)
                log(tb, prefix=log_prefix, file=run_log)

        return container

    @staticmethod
    def __stop_container(container):
        try:
            container.stop(timeout=2)
            time.sleep(2)
        except:
            # container has already been killed
            pass

    @staticmethod
    def __stop_all(docker_client):
        for container in docker_client.containers.list():
            if len(container.image.tags) > 0 \
                    and 'techempower' in container.image.tags[0] \
                    and 'tfb:latest' not in container.image.tags[0]:
                DockerHelper.__stop_container(container)

    def stop(self, containers=None):
        '''
        Attempts to stop a container or list of containers.
        If no containers are passed, stops all running containers.
        '''
        is_multi_setup = self.benchmarker.config.server_docker_host != \
                         self.benchmarker.config.database_docker_host

        if containers:
            if not isinstance(containers, list):
                containers = [containers]
            for container in containers:
                DockerHelper.__stop_container(container)
        else:
            DockerHelper.__stop_all(self.server)
            if is_multi_setup:
                DockerHelper.__stop_all(self.database)
                DockerHelper.__stop_all(self.client)

        self.database.containers.prune()
        if is_multi_setup:
            # Then we're on a 3 machine set up
            self.server.containers.prune()
            self.client.containers.prune()

    def build_databases(self):
        '''
        Builds all the databases necessary to run the list of benchmarker tests
        '''
        built = []
        for test in self.benchmarker.tests:
            db = test.database.lower()
            if db not in built and db != "none":
                image_name = "techempower/%s:latest" % db
                log_prefix = image_name + ": "

                database_dir = os.path.join(self.benchmarker.config.db_root,
                                            db)
                docker_file = "%s.dockerfile" % db

                self.__build(
                    base_url=self.benchmarker.config.database_docker_host,
                    path=database_dir,
                    dockerfile=docker_file,
                    log_prefix=log_prefix,
                    build_log_file=os.devnull,
                    tag="techempower/%s" % db)
                built.append(db)

    def start_database(self, database):
        '''
        Sets up a container for the given database and port, and starts said docker
        container.
        '''
        image_name = "techempower/%s:latest" % database
        log_prefix = image_name + ": "

        if self.benchmarker.config.network_mode is None:
            sysctl = {
                'net.core.somaxconn': 65535,
                'kernel.sem': "250 32000 256 512"
            }
        else:
            # Do not pass `net.*` kernel params when using host network mode
            sysctl = {
                'kernel.sem': "250 32000 256 512"
            }

        ulimit = [{'name': 'nofile', 'hard': 65535, 'soft': 65535}]

        container = self.database.containers.run(
            "techempower/%s" % database,
            name="tfb-database",
            network=self.benchmarker.config.network,
            network_mode=self.benchmarker.config.network_mode,
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
            database_ready = databases[database].test_connection(self.benchmarker.config)

        if not database_ready:
            log("Database was not ready after startup", prefix=log_prefix)

        return container

    def build_wrk(self):
        '''
        Builds the techempower/tfb.wrk container
        '''
        self.__build(
            base_url=self.benchmarker.config.client_docker_host,
            path=self.benchmarker.config.wrk_root,
            dockerfile="wrk.dockerfile",
            log_prefix="wrk: ",
            build_log_file=os.devnull,
            tag="techempower/tfb.wrk")

    def test_client_connection(self, url):
        '''
        Tests that the app server at the given url responds successfully to a
        request.
        '''
        try:
            self.client.containers.run(
                'techempower/tfb.wrk',
                'curl --fail --max-time 5 %s' % url,
                remove=True,
                log_config={'type': None},
                network=self.benchmarker.config.network,
                network_mode=self.benchmarker.config.network_mode)
        except Exception:
            return False

        return True

    def server_container_exists(self, container_id_or_name):
        '''
        Returns True if the container still exists on the server.
        '''
        try:
            self.server.containers.get(container_id_or_name)
            return True
        except:
            return False

    def benchmark(self, script, variables, raw_file):
        '''
        Runs the given remote_script on the wrk container on the client machine.
        '''

        def watch_container(container):
            with open(raw_file, 'w') as benchmark_file:
                for line in container.logs(stream=True):
                    log(line, file=benchmark_file)

        if self.benchmarker.config.network_mode is None:
            sysctl = {'net.core.somaxconn': 65535}
        else:
            # Do not pass `net.*` kernel params when using host network mode
            sysctl = None

        ulimit = [{'name': 'nofile', 'hard': 65535, 'soft': 65535}]

        watch_container(
            self.client.containers.run(
                "techempower/tfb.wrk",
                "/bin/bash /%s" % script,
                environment=variables,
                network=self.benchmarker.config.network,
                network_mode=self.benchmarker.config.network_mode,
                detach=True,
                stderr=True,
                ulimits=ulimit,
                sysctls=sysctl,
                remove=True,
                log_config={'type': None}))
