FROM buildpack-deps:xenial

RUN apt update -yqq && apt install -yqq software-properties-common apt-transport-https
RUN wget -qO- https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN add-apt-repository "$(wget -qO- https://packages.microsoft.com/config/ubuntu/16.04/prod.list)"

RUN apt update -yqq && ACCEPT_EULA=Y apt -qqy install \
  git-core \
  cloc dstat                    `# Collect resource usage statistics` \
  python-dev \
  python-pip \
  python-software-properties \
  libmysqlclient-dev            `# Needed for MySQL-python` \
  libpq-dev                     `# Needed for psycopg2` \
  msodbcsql17 unixodbc-dev      `# Needed for pyodbc + SQL Server`

RUN pip install colorama==0.3.1 requests MySQL-python pyodbc psycopg2-binary pymongo docker==3.1.0

ENV PYTHONPATH /FrameworkBenchmarks
ENV FWROOT /FrameworkBenchmarks

ENTRYPOINT ["python", "/FrameworkBenchmarks/toolset/run-tests.py"]
