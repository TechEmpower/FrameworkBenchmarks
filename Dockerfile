FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
# WARNING: DON'T PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
# One -q produces output suitable for logging (mostly hides
# progress indicators)
RUN apt-get install \
      --no-install-recommends \
      -o Dpkg::Options::="--force-confdef" \
      -o Dpkg::Options::="--force-confold" \
      -qqUy \
      cloc \
      curl \
      gcc \
      git-core \
      gosu \
      iproute2 \
      # Needed for mysqlclient
      libmysqlclient-dev \
      libpq-dev \
      pkg-config \
      python3 \
      python3-colorama \
      python3-dev \
      python3-dnspython \
      python3-packaging \
      python3-pip \
      python3-psutil \
      python3-psycopg2 \
      python3-requests \
      siege \
      software-properties-common && \
    # Ubuntu's equivalent packages are too old and/or broken.
    pip3 install \
      --break-system-packages \
      docker==7.0.0 \
      mysqlclient==2.2.4 \
      pymongo==4.7.2

# Collect resource usage statistics
ARG DOOL_VERSION=v1.3.1

WORKDIR /tmp
RUN curl -LSs "https://github.com/scottchiefbaker/dool/archive/${DOOL_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    ./install.py

# create group and user
ARG GROUP_ID
ARG USER_ID

RUN groupadd -g "$GROUP_ID" user || true && \
    useradd -m -u "$USER_ID" -g "$GROUP_ID" -s /bin/bash user || true

ENV FWROOT=/FrameworkBenchmarks USER_ID="$USER_ID"
ENV PYTHONPATH="$FWROOT"

ENTRYPOINT ["/FrameworkBenchmarks/entrypoint.sh"]
