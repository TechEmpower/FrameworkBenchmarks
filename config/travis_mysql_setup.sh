sed -i 's|\[mysqld\]|\[mysqld\]\
lower_case_table_names = 1\
character-set-server=utf8\
collation-server=utf8_general_ci|g' /etc/mysql/my.cnf
