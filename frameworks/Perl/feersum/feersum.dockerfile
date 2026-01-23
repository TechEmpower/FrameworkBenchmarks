FROM perl:5.42
RUN apt-get update
RUN apt-get install -y --no-install-recommends --no-install-suggests catatonit libmariadb-dev libpq-dev libev-dev liblmdb-dev build-essential curl gnupg
RUN curl -fsSL https://raw.githubusercontent.com/skaji/cpm/main/cpm | perl - install -g App::cpm
WORKDIR /app
ADD cpanfile .
RUN cpm install -g

RUN curl https://repo.mysql.com/RPM-GPG-KEY-mysql-2023 -o /etc/apt/trusted.gpg.d/mysql2023
RUN gpg --dearmor /etc/apt/trusted.gpg.d/mysql2023
RUN rm /etc/apt/trusted.gpg.d/mysql2023
RUN echo 'deb http://repo.mysql.com/apt/debian bookworm mysql-innovation' > /etc/apt/sources.list.d/mysql.list
RUN apt-get update
RUN apt-get install  -y --no-install-recommends --no-install-suggests libmysqlclient-dev
add cpanfile_alt .
RUN cpm install -g --cpanfile=cpanfile_alt

RUN apt-get clean
RUN rm -rf $HOME/.perl-cpm
ADD app.pl .
EXPOSE 8080

ARG TFB_TEST_DATABASE
ENV db=$TFB_TEST_DATABASE

STOPSIGNAL SIGKILL

CMD perl app.pl
