FROM tfb/haskell:latest

COPY ./ ./

RUN stack --allow-different-user setup
RUN stack --allow-different-user build

CMD stack --allow-different-user exec servant-exe -- TFB-database +RTS -A32m -N$(nproc)
