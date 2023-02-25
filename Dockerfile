FROM ubuntu:18.04

ARG DEBIAN_FRONTEND=noninteractive
# WARNING: DON'T PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
# One -q produces output suitable for logging (mostly hides
# progress indicators)
RUN apt-get -yqq update && apt-get -yqq install \
      -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
      cloc \
      dstat                       `# Collect resource usage statistics` \
      git-core \
      libmysqlclient-dev          `# Needed for MySQL-python` \
      python-dev \
      python-pip \
      siege \
      software-properties-common && \
    pip install \
      colorama==0.3.1 \
      docker==4.0.2 \
      MySQL-python \
      psutil \
      psycopg2-binary \
      pymongo \
      requests && \
    # Fix for docker-py trying to import one package from the wrong location
    cp -r /usr/local/lib/python2.7/dist-packages/backports/ssl_match_hostname \
      /usr/lib/python2.7/dist-packages/backports

ENV FWROOT=/FrameworkBenchmarks PYTHONPATH=/FrameworkBenchmarks

ENTRYPOINT ["python", "/FrameworkBenchmarks/toolset/run-tests.py"]
