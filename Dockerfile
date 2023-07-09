FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive
# WARNING: DON'T PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
# One -q produces output suitable for logging (mostly hides
# progress indicators)
RUN apt-get -yqq update && \
    apt-get -yqq install \
      -o Dpkg::Options::="--force-confdef" \
      -o Dpkg::Options::="--force-confold" \
      cloc \
      curl \
      # Collect resource usage statistics
      dstat \
      gcc \
      git-core \
      gosu \
      # Needed for mysqlclient
      libmysqlclient-dev \
      libpq-dev \
      pkg-config \
      python3 \
      python3-dev \
      python3-pip \
      siege \
      software-properties-common

RUN pip3 install \
      colorama==0.3.1 \
      docker==4.0.2 \
      mysqlclient \
      psutil \
      psycopg2-binary \
      pymongo==3.13.0 \
      # urllib3 incompatibility:
      # https://github.com/docker/docker-py/issues/3113#issuecomment-1525500104
      requests==2.28.1

# Check if the group ID is already created
ARG GROUP_ID
RUN if ! getent group "$GROUP_ID"; then \
      addgroup --gid "$GROUP_ID" user; \
    fi

# Check if the user ID is already created
ARG USER_ID
RUN if ! getent passwd "$USER_ID"; then \
      adduser --disabled-password --gecos '' --gid "$GROUP_ID" --uid "$USER_ID" user; \
    fi

ENV FWROOT=/FrameworkBenchmarks PYTHONPATH=/FrameworkBenchmarks USER_ID="$USER_ID"

ENTRYPOINT ["FrameworkBenchmarks/entrypoint.sh"]
