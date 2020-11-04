FROM sumeetchhetri/ffead-cpp-5.0-base:5.2

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-5.0 nginx mongo
