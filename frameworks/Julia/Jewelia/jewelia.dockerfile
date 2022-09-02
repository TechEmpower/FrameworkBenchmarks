FROM ubuntu:latest

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -yqq && apt-get install -y
RUN apt-get update -yqq && apt-get install -y wget

RUN wget https://julialang-s3.julialang.org/bin/linux/x64/1.6/julia-1.6.2-linux-x86_64.tar.gz
RUN tar zxvf julia-1.6.2-linux-x86_64.tar.gz
ENV PATH="$PATH:/julia-1.6.2/bin"

RUN rm -f julia-1.6.2-linux-x86_64.tar.gz

COPY ./ ./

RUN julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.instantiate()' && julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.precompile()'

RUN chmod +x run.sh

EXPOSE 8080

CMD ./run.sh
