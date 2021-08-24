FROM buildpack-deps:stretch

RUN apt-get update && apt-get install apt-transport-https
RUN curl https://contrastsecurity.jfrog.io/contrastsecurity/api/gpg/key/public | apt-key add -
RUN echo "deb https://contrastsecurity.jfrog.io/contrastsecurity/debian-public/ stretch contrast" | tee /etc/apt/sources.list.d/contrastc.list
RUN apt-get update && apt-get install contrast-service

COPY ./contrast_security.yaml /etc/contrast/contrast_security.yaml

RUN mv /etc/contrast/webserver/contrast_security.yaml /tmp

RUN mkdir -p /host/var/log

CMD /usr/bin/contrast-service > /host/var/log/contrast-service.log
