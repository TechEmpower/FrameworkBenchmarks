FROM python:3.6.6-stretch

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip3 install -r /pyramid/requirements.txt

EXPOSE 8080


# Start Contrast Additions
COPY contrast-agent.tar.gz contrast-agent.tar.gz
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=false
ENV CONTRAST__PROTECT__ENABLE=false

run apt-get update && apt-get install apt-transport-https
run curl https://contrastsecurity.jfrog.io/contrastsecurity/api/gpg/key/public | apt-key add -
run echo "deb https://contrastsecurity.jfrog.io/contrastsecurity/debian-public/ stretch contrast" | tee /etc/apt/sources.list.d/contrastc.list
run apt-get update && apt-get install contrast-service

run pip3 install ./contrast-agent.tar.gz
# End Contrast Additions

CMD gunicorn wsgi-contrast:app -c gunicorn_conf.py
