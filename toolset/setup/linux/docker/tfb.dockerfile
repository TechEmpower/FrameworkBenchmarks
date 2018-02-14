FROM ubuntu:14.04

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8 

RUN apt-get update
RUN apt-get install -qqy software-properties-common build-essential curl

# ENV CARGO_HOME=/usr
# RUN curl https://sh.rustup.rs -o rustup.sh && chmod 777 rustup.sh
# RUN ./rustup.sh -y
# RUN rm ./rustup.sh

ADD TFBReaper TFBReaper

# RUN cargo build --manifest-path TFBReaper/Cargo.toml

RUN mv TFBReaper/target/debug/tfb_reaper /

ENTRYPOINT ["/tfb_reaper"]