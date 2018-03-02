import os
import docker
import fnmatch
import subprocess
import multiprocessing


def clean(self):
    '''
    Cleans all the docker images from the system
    '''
    subprocess.check_call(["docker", "image", "prune", "-f"])

    docker_ids = subprocess.check_output(["docker", "images",
                                          "-q"]).splitlines()
    for docker_id in docker_ids:
        subprocess.check_call(["docker", "image", "rmi", "-f", docker_id])


def build_docker_images(config):

    print(config.build)
    return
    '''
    Builds the dependency chain as well as the test implementation docker images
    for the given tests.
    '''
    # docker_buildargs = {
    #     'CPU_COUNT': str(multiprocessing.cpu_count()),
    #     'MAX_CONCURRENCY': str(max(self.benchmarker.concurrency_levels))
    # }

    # test_docker_files = ["%s.dockerfile" % self.name]
    # if self.docker_files is not None:
    #     if type(self.docker_files) is list:
    #         test_docker_files.extend(self.docker_files)
    #     else:
    #         raise Exception(
    #             "docker_files in benchmark_config.json must be an array")

    # for test_docker_file in test_docker_files:
    #     deps = list(
    #         reversed(
    #             gather_docker_dependencies(
    #                 os.path.join(self.directory, test_docker_file))))

    #     docker_dir = os.path.join(setup_util.get_fwroot(), "toolset", "setup",
    #                               "linux", "docker")

    #     for dependency in deps:
    #         docker_file = os.path.join(self.directory,
    #                                    dependency + ".dockerfile")
    #         if not docker_file or not os.path.exists(docker_file):
    #             docker_file = find_docker_file(docker_dir,
    #                                            dependency + ".dockerfile")
    #         if not docker_file:
    #             tee_output(
    #                 prefix,
    #                 "Docker build failed; %s could not be found; terminating\n"
    #                 % (dependency + ".dockerfile"))
    #             return 1

    #         # Build the dependency image
    #         try:
    #             for line in docker.APIClient(
    #                     base_url='unix://var/run/docker.sock').build(
    #                         path=os.path.dirname(docker_file),
    #                         dockerfile="%s.dockerfile" % dependency,
    #                         tag="tfb/%s" % dependency,
    #                         buildargs=docker_buildargs,
    #                         forcerm=True):
    #                 handle_build_output(line)
    #         except Exception as e:
    #             tee_output(prefix,
    #                        "Docker dependency build failed; terminating\n")
    #             print(e)
    #             return 1

    # # Build the test images
    # for test_docker_file in test_docker_files:
    #     try:
    #         for line in docker.APIClient(
    #                 base_url='unix://var/run/docker.sock').build(
    #                     path=self.directory,
    #                     dockerfile=test_docker_file,
    #                     tag="tfb/test/%s" % test_docker_file.replace(
    #                         ".dockerfile", ""),
    #                     buildargs=docker_buildargs,
    #                     forcerm=True):
    #             handle_build_output(line)
    #     except Exception as e:
    #         tee_output(prefix, "Docker build failed; terminating\n")
    #         print(e)
    #         return 1


def find_docker_file(path, pattern):
    '''
  Finds and returns all the the files matching the given pattern recursively in
  the given path. 
  '''
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                return os.path.join(root, name)


def gather_docker_dependencies(docker_file):
    '''
  Gathers all the known docker dependencies for the given docker image.
  '''
    # Avoid setting up a circular import
    from toolset.setup.linux import setup_util
    deps = []

    docker_dir = os.path.join(setup_util.get_fwroot(), "toolset", "setup",
                              "linux", "docker")

    if os.path.exists(docker_file):
        with open(docker_file) as fp:
            for line in fp.readlines():
                tokens = line.strip().split(' ')
                if tokens[0] == "FROM":
                    # This is magic that our base image points to
                    if tokens[1] != "ubuntu:16.04":
                        depToken = tokens[1].strip().split(':')[
                            0].strip().split('/')[1]
                        deps.append(depToken)
                        dep_docker_file = os.path.join(
                            os.path.dirname(docker_file),
                            depToken + ".dockerfile")
                        if not os.path.exists(dep_docker_file):
                            dep_docker_file = find_docker_file(
                                docker_dir, depToken + ".dockerfile")
                        deps.extend(
                            gather_docker_dependencies(dep_docker_file))

    return deps
