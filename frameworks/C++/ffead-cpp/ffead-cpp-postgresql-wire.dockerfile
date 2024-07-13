FROM sumeetchhetri/ffead-cpp-base:7.0-te

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-7.0-sql emb postgresql-wire memory
