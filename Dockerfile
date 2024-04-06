FROM ubuntu:24.04

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
      python3-psutil \
      python3-psycopg2 \
      python3-requests \
      siege \
      software-properties-common && \
    # Ubuntu's equivalent packages are too old and/or broken.
    pip3 install \
      --break-system-packages \
      colorama==0.3.1 \
      docker==7.0.0 \
      mysqlclient==2.2.4 \
      pymongo==3.13.0

# Collect resource usage statistics
ARG DOOL_VERSION=v1.3.1

WORKDIR /tmp
RUN curl -LSs "https://github.com/scottchiefbaker/dool/archive/${DOOL_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    ./install.py

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

ENV FWROOT=/FrameworkBenchmarks USER_ID="$USER_ID"
ENV PYTHONPATH="$FWROOT"

ENTRYPOINT ["/FrameworkBenchmarks/entrypoint.sh"]
