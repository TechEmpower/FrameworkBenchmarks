FROM python:3.6.6-stretch

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip3 install -r /pyramid/requirements.txt

EXPOSE 8080

# Start Contrast Additions
COPY contrast-agent.tar.gz contrast-agent.tar.gz
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=false
ENV CONTRAST__PROTECT__ENABLE=true

run pip3 install ./contrast-agent.tar.gz
# End Contrast Additions/Alterations

# Uses alternate gunicorn config
CMD gunicorn wsgi-contrast:app -c gunicorn_conf-contrast.py
