#!/bin/sh
if [ -f /etc/mysql/my.cnf ]; then
    sudo rm /etc/mysql/my.cnf
fi

# Assumes file settings are changed by this point

# Incorporate this Wednesday >>
# vagrant@TFB-all:/var/log$ sudo mkdir mysql
# vagrant@TFB-all:/var/log$ sudo touch mysql/mysql.log
# vagrant@TFB-all:/var/log$ sudo touch mysql/mysql.error.log


# sudo chmod 0644 my.cnf
# sudo cp my.cnf /etc/mysql/my.cnf
# sudo service mysql restart

echo "Shell script ran"
echo $0
echo $1

return 0