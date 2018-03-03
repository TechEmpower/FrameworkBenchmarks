FROM tfb/yesod-mongodb-base:latest

CMD stack --allow-different-user exec yesod-mysql-mongo -- ${CPU_COUNT} ${DBHOST} +RTS -A32m -N${CPU_COUNT}
