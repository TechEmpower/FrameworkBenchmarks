FROM sumeetchhetri/ffead-cpp-sql-raw-profiled-base:6.0

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0-sql emb postgresql-raw memory
