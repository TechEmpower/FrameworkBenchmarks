FROM sumeetchhetri/ffead-cpp-v-base:7.0-te

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-7.0-sql v-picov postgresql-raw-async memory
