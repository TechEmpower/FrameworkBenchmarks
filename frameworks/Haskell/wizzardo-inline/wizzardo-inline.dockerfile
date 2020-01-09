FROM tweag/linear-types:1.0.5
MAINTAINER Facundo Dominguez <facundo.dominguez@tweag.io>

RUN apt-get update && apt-get install -y gradle openjdk-8-jdk

RUN git clone https://github.com/tweag/inline-java && \
    cd inline-java && \
    git checkout 5b2552f && \
	bash -c ". env-linear.sh; stack build inline-java"

RUN cd inline-java && \
	bash -c ". env-linear.sh; stack build wizzardo-http-benchmark"

CMD bash -c "cd inline-java; . env-linear.sh; stack exec -- wizzardo-http-benchmark env=prod +RTS -A32m -N$(nproc) -RTS"
