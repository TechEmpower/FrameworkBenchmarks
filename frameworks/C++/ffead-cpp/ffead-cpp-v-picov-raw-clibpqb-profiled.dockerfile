FROM sumeetchhetri/ffead-cpp-v-picov-raw-clibpqb-profiled-base:6.0

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0-sql v-picov postgresql-raw memory
