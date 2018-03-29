FROM techempower/yesod-mongodb-base:0.1

CMD stack --allow-different-user exec yesod-mysql-mongo -- ${CPU_COUNT} tfb-database +RTS -A32m -N${CPU_COUNT}
