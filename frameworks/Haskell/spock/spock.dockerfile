FROM techempower/haskell:0.1

COPY ./ ./

RUN stack --allow-different-user build --install-ghc

CMD stack --allow-different-user exec spock-exe -- +RTS -A32m -N${CPU_COUNT}
