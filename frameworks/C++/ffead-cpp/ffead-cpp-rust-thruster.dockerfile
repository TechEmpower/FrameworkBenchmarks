FROM sumeetchhetri/ffead-cpp-rust-base:latest

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 rust-thruster
