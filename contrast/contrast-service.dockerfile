FROM buildpack-deps:stretch

run apt-get update && apt-get install apt-transport-https
run curl https://contrastsecurity.jfrog.io/contrastsecurity/api/gpg/key/public | apt-key add -
run echo "deb https://contrastsecurity.jfrog.io/contrastsecurity/debian-public/ stretch contrast" | tee /etc/apt/sources.list.d/contrastc.list
run apt-get update && apt-get install contrast-service

COPY ./contrast_security.yaml /etc/contrast/contrast_security.yaml

run mv /etc/contrast/webserver/contrast_security.yaml /tmp

CMD /usr/bin/contrast-service