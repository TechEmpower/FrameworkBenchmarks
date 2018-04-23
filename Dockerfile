FROM ubuntu:16.04

# One -q produces output suitable for logging (mostly hides
# progress indicators)
RUN apt update -yqq

# WARNING: DONT PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
RUN apt -qqy install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
  git-core \
  cloc dstat                    `# Collect resource usage statistics` \
  python-dev \
  python-pip \
  python-software-properties \
  libmysqlclient-dev            `# Needed for MySQL-python` \
  libpq-dev                     `# Needed for psycopg2`

RUN pip install colorama==0.3.1 requests MySQL-python psycopg2-binary pymongo docker==3.1.0

ENV PYTHONPATH /FrameworkBenchmarks
ENV FWROOT /FrameworkBenchmarks

ENTRYPOINT ["python", "/FrameworkBenchmarks/toolset/run-tests.py"]
