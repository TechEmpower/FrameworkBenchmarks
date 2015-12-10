#!/bin/sh

cd "$(dirname "$0")"

if [ -f /etc/mysql/my.cnf ]; then
    sudo rm /etc/mysql/my.cnf
fi


LOG=/tmp/tfb-temporary-mysql.log

if [ -f $LOG ]; then
	sudo rm $LOG
fi
# Cannot initialize log in its final location because that is a synced folder, so
touch $LOG                                   # create a log file
sudo chown mysql:mysql $LOG                  # in a location that mysql can own

cp my.cnf my.sql-logging.cnf                 # Start with my.cnf
sed -i "s|.*general_log_file.*|general_log_file = ${LOG}|" my.sql-logging.cnf
sed -i 's|.*general_log .*|general_log = 1|' my.sql-logging.cnf

sudo mv my.sql-logging.cnf /etc/mysql/my.cnf # Move file
sudo chmod 0644 /etc/mysql/my.cnf            # before changing modes

sudo service mysql restart

return 0