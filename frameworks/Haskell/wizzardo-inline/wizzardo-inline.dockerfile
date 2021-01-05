FROM tweag/linear-types:1.0.5
MAINTAINER Facundo Dominguez <facundo.dominguez@tweag.io>

RUN apt-get update && apt-get install -y gradle openjdk-8-jdk

USER root
WORKDIR /wizzardo-inline

COPY wizzardo-http-benchmark wizzardo-http-benchmark
COPY stack-linear.yaml stack-linear.yaml
COPY env-linear.sh env-linear.sh
COPY set-java-home.sh set-java-home.sh

RUN stack upgrade

RUN bash -c ". env-linear.sh; stack build wizzardo-http-benchmark --no-terminal"

EXPOSE 8080

CMD bash -c ". env-linear.sh; stack exec -- wizzardo-http-benchmark env=prod +RTS -A32m -N$(nproc) -RTS"
