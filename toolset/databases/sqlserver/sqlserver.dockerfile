FROM buildpack-deps:xenial

RUN apt update -yqq && apt install -yqq software-properties-common apt-transport-https locales

RUN locale-gen en_US.UTF-8
RUN update-locale LANG=en_US.UTF-8

RUN wget -qO- https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN add-apt-repository "$(wget -qO- https://packages.microsoft.com/config/ubuntu/16.04/mssql-server-2017.list)"
RUN add-apt-repository "$(wget -qO- https://packages.microsoft.com/config/ubuntu/16.04/prod.list)"

RUN apt update -yqq && ACCEPT_EULA=Y apt install -yqq mssql-server mssql-tools unixodbc-dev

WORKDIR /sqlserver
COPY create.sql create.sql

RUN SA_PASSWORD=OneTwoThree123! ACCEPT_EULA=Y nohup /opt/mssql/bin/sqlservr & \
    until /opt/mssql-tools/bin/sqlcmd -S localhost -U SA -P 'OneTwoThree123!' -Q "select @@VERSION"; do sleep 1; done && \
    /opt/mssql-tools/bin/sqlcmd -S localhost -U SA -P 'OneTwoThree123!' -i create.sql

CMD ACCEPT_EULA=Y /opt/mssql/bin/sqlservr
