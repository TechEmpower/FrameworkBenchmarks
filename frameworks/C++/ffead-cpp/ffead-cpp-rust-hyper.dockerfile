FROM sumeetchhetri/ffead-cpp-5.0-rust-base:5.3

ENV IROOT=/installs

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-5.0 rust-hyper
