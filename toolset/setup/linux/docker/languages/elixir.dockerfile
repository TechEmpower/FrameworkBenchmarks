FROM tfb/erlang:latest

ENV ELIXIR_VERSION="1.5.2-1"
ENV RELEASE="xenial"
ENV ARCH="amd64"

RUN wget http://packages.erlang-solutions.com/erlang/elixir/FLAVOUR_2_download/elixir_${ELIXIR_VERSION}~ubuntu~${RELEASE}_${ARCH}.deb
RUN dpkg -x elixir_${ELIXIR_VERSION}~ubuntu~${RELEASE}_${ARCH}.deb /elixir
RUN /erlang/usr/lib/erlang/Install -minimal /erlang/usr/lib/erlang

ENV PATH=elixir/usr/local/bin:${PATH}
