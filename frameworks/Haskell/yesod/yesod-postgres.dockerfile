FROM tfb/haskell:latest

COPY ./yesod-postgres ./

RUN stack --allow-different-user build --install-ghc

CMD stack --allow-different-user exec yesod-postgres -- ${CPU_COUNT} TFB-database +RTS -A32m -N${CPU_COUNT}
