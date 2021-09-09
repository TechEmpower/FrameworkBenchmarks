FROM ubuntu:latest

ENV DEBIAN_FRONTEND interactive

RUN apt-get update -yqq && apt-get install -y
RUN apt-get update -yqq && apt-get install -y wget

RUN mkdir /usr/Downloads

WORKDIR /usr/Downloads

RUN wget https://julialang-s3.julialang.org/bin/linux/x64/1.6/julia-1.6.2-linux-x86_64.tar.gz
RUN tar zxvf julia-1.6.2-linux-x86_64.tar.gz

ENV PATH="$PATH:/usr/Downloads/julia-1.6.2/bin"

RUN mkdir /jewelia
ADD ./ /jewelia

RUN julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.instantiate()' && julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.precompile()'

