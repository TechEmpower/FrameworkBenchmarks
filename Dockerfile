FROM buildpack-deps:xenial

# One -q produces output suitable for logging (mostly hides
# progress indicators)
RUN apt update -yqq

# WARNING: DONT PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
RUN apt -qqy install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
  cloc dstat                    `# Collect resource usage statistics` \
  python-dev \
  python-pip \
  python-software-properties \
  libmysqlclient-dev            `# Needed for MySQL-python`

RUN pip install colorama==0.3.1 requests MySQL-python psycopg2-binary pymongo docker==3.1.0

ENV PYTHONPATH /FrameworkBenchmarks
ENV FWROOT /FrameworkBenchmarks

ENTRYPOINT ["python", "/FrameworkBenchmarks/toolset/run-tests.py"]
