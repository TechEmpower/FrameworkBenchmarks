FROM debian:bullseye-slim

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -yqq && apt-get install -y
RUN apt-get update -yqq && apt-get install -y wget

WORKDIR /genie

RUN wget https://julialang-s3.julialang.org/bin/linux/x64/1.7/julia-1.7.3-linux-x86_64.tar.gz
RUN tar zxvf julia-1.7.3-linux-x86_64.tar.gz
ENV PATH="$PATH:/genie/julia-1.7.3/bin"

RUN rm -f julia-1.7.3-linux-x86_64.tar.gz

COPY ./ ./

RUN julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.instantiate()' && \
    julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.precompile()'

RUN chmod +x bin/repl
RUN chmod +x bin/server
RUN chmod +x bin/runtask

ENV GENIE_HOST "0.0.0.0"
ENV GENIE_ENV "prod"

EXPOSE 8080

CMD ["bin/server"]
