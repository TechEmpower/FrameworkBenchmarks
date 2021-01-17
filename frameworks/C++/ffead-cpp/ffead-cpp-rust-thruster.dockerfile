FROM sumeetchhetri/ffead-cpp-5.0-rust-base:latest

ENV IROOT=/installs

WORKDIR /

EXPOSE 8080

CMD ./run_ffead.sh ffead-cpp-5.0 rust-thruster
