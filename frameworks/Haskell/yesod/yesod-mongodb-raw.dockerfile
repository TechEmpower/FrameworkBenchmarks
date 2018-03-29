FROM tfb/yesod-mongodb-base:latest

CMD stack --allow-different-user exec yesod-mysql-mongo -- $(nproc) TFB-database +RTS -A32m -N$(nproc)
