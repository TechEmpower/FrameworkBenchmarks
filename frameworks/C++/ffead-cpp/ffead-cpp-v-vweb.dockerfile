FROM sumeetchhetri/ffead-cpp-5.0-v-base:latest

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-5.0 v-vweb
