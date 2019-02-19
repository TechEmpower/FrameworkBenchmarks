set -e

AUTH_FILE=`psql -U postgres -c "SHOW hba_file" -At`

# md5 authentication

sudo sed -i '1s/^/host	all	crystal_md5	127.0.0.1\/32	md5\n/' ${AUTH_FILE}
sudo sed -i '2s/^/host	all	crystal_md5	::1\/128	md5\n/' ${AUTH_FILE}

sudo service postgresql restart $TRAVIS_POSTGRESQL_VERSION
