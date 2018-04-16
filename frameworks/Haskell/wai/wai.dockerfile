FROM haskell:8.2.1

RUN apt update -yqq && apt install -yqq xz-utils make

ADD ./ /wai
WORKDIR /wai

RUN stack upgrade

RUN cd bench && stack --allow-different-user build --install-ghc

CMD cd bench && stack --allow-different-user exec bench -- $(nproc) tfb-database +RTS -A32m -N$(nproc)
