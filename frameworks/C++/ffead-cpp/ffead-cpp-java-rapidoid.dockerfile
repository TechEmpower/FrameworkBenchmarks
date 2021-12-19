FROM sumeetchhetri/ffead-cpp-java-base:6.0

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 java-rapidoid
