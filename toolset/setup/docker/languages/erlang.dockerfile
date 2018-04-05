FROM techempower/base:0.1

ENV ERLANG_VERSION="18.2-1"
ENV RELEASE="xenial"
ENV ARCH="amd64"

RUN wget https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_${ERLANG_VERSION}~ubuntu~${RELEASE}_${ARCH}.deb
RUN dpkg -x esl-erlang_${ERLANG_VERSION}~ubuntu~${RELEASE}_${ARCH}.deb /erlang
RUN /erlang/usr/lib/erlang/Install -minimal /erlang/usr/lib/erlang

RUN apt install -yqq rebar

ENV PATH=/erlang/usr/bin:${PATH}
