FROM sumeetchhetri/ffead-cpp-5.0-java-base:latest

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-5.0 java-wizzardo-http
