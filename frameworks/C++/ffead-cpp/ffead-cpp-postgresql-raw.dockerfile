FROM sumeetchhetri/ffead-cpp-5.0-base:5.3

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-5.0-sql emb postgresql-raw memory
