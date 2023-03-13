FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

#RUN add-apt-repository universe
# WARNING: DON'T PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
# One -q produces output suitable for logging (mostly hides
# progress indicators)
RUN apt-get -yqq update && apt-get -yqq install \
      -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
      cloc \
      dstat                       `# Collect resource usage statistics` \
      git-core \
      libmysqlclient-dev          `# Needed for MySQL-python` \
      libpq-dev \
      python2 \
      python2.7-dev \
      curl \
      siege \
      gcc \
      software-properties-common
RUN curl https://bootstrap.pypa.io/pip/2.7/get-pip.py --output get-pip.py
RUN python2 get-pip.py

RUN curl https://raw.githubusercontent.com/paulfitz/mysql-connector-c/master/include/my_config.h --output /usr/include/mysql/my_config.h

RUN pip install \
      colorama==0.3.1 \
      docker==4.0.2 \
      MySQL-python \
      psutil \
      psycopg2-binary \
      pymongo \
      requests
    # Fix for docker-py trying to import one package from the wrong location
    #cp -r /usr/local/lib/python2.7/dist-packages/backports/ssl_match_hostname \
    #  /usr/lib/python2.7/dist-packages/backports

ENV FWROOT=/FrameworkBenchmarks PYTHONPATH=/FrameworkBenchmarks

ENTRYPOINT ["python2", "/FrameworkBenchmarks/toolset/run-tests.py"]
