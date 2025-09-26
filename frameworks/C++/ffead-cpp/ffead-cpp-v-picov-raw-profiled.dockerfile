FROM sumeetchhetri/ffead-cpp-v-picov-raw-profiled-base:6.0

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0-sql v-picov postgresql-raw memory
