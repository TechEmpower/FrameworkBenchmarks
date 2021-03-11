FROM python:3.9.1-buster

ADD ./ /django

WORKDIR /django

RUN pip install -r /django/requirements.txt

EXPOSE 8080

# Start Contrast Additions
COPY contrast-agent.tar.gz contrast-agent.tar.gz
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=true
ENV CONTRAST__PROTECT__ENABLE=true

run pip3 install ./contrast-agent.tar.gz
# End Contrast Additions

# Uses alternate gunicorn config
CMD gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf-contrast.py --env DJANGO_DB=mysql
