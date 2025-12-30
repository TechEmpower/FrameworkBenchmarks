from perl:5.40-slim
run apt-get update
run apt-get install -y --no-install-recommends --no-install-suggests catatonit libmariadb-dev libpq-dev libev-dev liblmdb-dev build-essential curl gnupg
run curl -fsSL https://raw.githubusercontent.com/skaji/cpm/main/cpm | perl - install -g App::cpm
workdir /app
add cpanfile .
run cpm install -g

run curl https://repo.mysql.com/RPM-GPG-KEY-mysql-2023 -o /etc/apt/trusted.gpg.d/mysql2023
run gpg --dearmor /etc/apt/trusted.gpg.d/mysql2023
run rm /etc/apt/trusted.gpg.d/mysql2023
run echo 'deb http://repo.mysql.com/apt/debian bookworm mysql-innovation' > /etc/apt/sources.list.d/mysql.list
run apt-get update
run apt-get install  -y --no-install-recommends --no-install-suggests libmysqlclient-dev
add cpanfile_alt .
run cpm install -g --cpanfile=cpanfile_alt

run apt-get clean
run rm -rf $HOME/.perl-cpm
add app.pl .
expose 8080

arg TFB_TEST_DATABASE
env db=$TFB_TEST_DATABASE

stopsignal SIGKILL

cmd perl app.pl
