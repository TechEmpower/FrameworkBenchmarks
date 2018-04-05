FROM techempower/haskell:0.1

COPY ./yesod-mysql-mongo ./

RUN stack --allow-different-user build --install-ghc
