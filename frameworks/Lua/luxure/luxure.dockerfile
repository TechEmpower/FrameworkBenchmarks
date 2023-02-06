FROM ubuntu:22.04

COPY ./server.lua /

RUN DEBIAN_FRONTEND=noninteractiv \
  apt-get update \
  && apt-get install -y \
  build-essential \
  libreadline-dev \
  unzip \
  curl \
  wget \
  libssl-dev \
  && curl -R -O http://www.lua.org/ftp/lua-5.3.5.tar.gz \
  && tar -zxf lua-5.3.5.tar.gz \
  && cd lua-5.3.5 \
  && make linux test \
  && make install \
  && cd .. \
  && rm -rf lua-5.3.5 \
  && rm ./lua-5.3.5.tar.gz \
  && wget https://luarocks.org/releases/luarocks-3.8.0.tar.gz \
  && tar zxpf luarocks-3.8.0.tar.gz \
  && cd luarocks-3.8.0 \
  && ./configure --with-lua-include=/usr/local/include \
  && make \
  && make install \
  && cd .. \
  && rm -rf ./luarocks-3.8.0 \
  && rm luarocks-3.8.0.tar.gz \
  && luarocks install luxure \
  && luarocks install dkjson

EXPOSE 8080

CMD lua /server.lua
