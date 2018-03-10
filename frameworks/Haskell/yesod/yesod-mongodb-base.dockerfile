FROM tfb/haskell:latest

COPY ./yesod-mysql-mongo ./

RUN stack --allow-different-user build --install-ghc
