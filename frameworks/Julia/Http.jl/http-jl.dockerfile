FROM ubuntu:20.04

ENV IROOT=/installs

ENV DEBIAN_FRONTEND noninteractive
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN apt update -yqq && apt-get install -y --reinstall ca-certificates
RUN apt-get update -y && apt-get install -y --no-install-recommends wget

RUN mkdir /usr/local/share/ca-certificates/cacert.org
RUN wget -P /usr/local/share/ca-certificates/cacert.org http://www.cacert.org/certs/root.crt http://www.cacert.org/certs/class3.crt
RUN update-ca-certificates
#RUN git config --global http.sslCAinfo /etc/ssl/certs/ca-certificates.crt

WORKDIR ${IROOT}

RUN wget -q https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-1.5.3-linux-x86_64.tar.gz
RUN tar -xzf julia-1.5.3-linux-x86_64.tar.gz
RUN mv julia-1.5.3 /opt/
RUN rm -f julia-1.5.3-linux-x86_64.tar.gz
ENV PATH="/opt/julia-1.5.3/bin:${PATH}"

COPY *.toml ${IROOT}/

RUN julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.instantiate()' && \
    julia -e 'import Pkg; Pkg.activate(@__DIR__); Pkg.precompile()'

COPY server.jl ${IROOT}/
COPY run.sh ${IROOT}/
RUN chmod +x run.sh

EXPOSE 8080

CMD ./run.sh
