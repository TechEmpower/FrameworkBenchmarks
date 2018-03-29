FROM tfb/haskell:latest

COPY ./ ./

RUN cd bench && stack --allow-different-user build --install-ghc

CMD cd bench && stack --allow-different-user exec bench -- $(nproc) TFB-database +RTS -A32m -N$(nproc)
