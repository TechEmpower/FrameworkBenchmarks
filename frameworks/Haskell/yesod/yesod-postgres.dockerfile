FROM techempower/haskell:0.1

COPY ./yesod-postgres ./

RUN stack --allow-different-user build --install-ghc

CMD stack --allow-different-user exec yesod-postgres -- $(nproc) tfb-database +RTS -A32m -N$(nproc)
