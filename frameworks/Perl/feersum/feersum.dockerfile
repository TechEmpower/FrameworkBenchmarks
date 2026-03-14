FROM perl:5.42
RUN apt-get update
RUN apt-get install -y --no-install-recommends --no-install-suggests catatonit default-libmysqlclient-dev libmariadb-dev libpq-dev libev-dev liblmdb-dev build-essential curl gnupg
RUN curl -fsSL https://raw.githubusercontent.com/skaji/cpm/main/cpm | perl - install -g App::cpm
WORKDIR /app
COPY cpanfile .
RUN cpm install -g --show-build-log-on-failure

COPY app.pl .
EXPOSE 8080

ARG TFB_TEST_DATABASE
ENV db=$TFB_TEST_DATABASE

STOPSIGNAL SIGKILL

CMD perl app.pl
