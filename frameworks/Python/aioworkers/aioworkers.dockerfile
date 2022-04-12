FROM python:3.9-alpine
RUN apk add --no-cache coreutils make gcc
RUN apk add --no-cache python3-dev musl-dev libffi-dev
RUN pip3 install uvloop

ADD ./requirements.txt /aioworkers/
ADD ./requirements-pg.txt /aioworkers/

WORKDIR /aioworkers

RUN pip3 install -U pip && \
    pip3 install -r /aioworkers/requirements-pg.txt

ADD ./ /aioworkers

EXPOSE 8080

CMD aioworkers aioworkers.net.web --multiprocessing -c config.yaml -c config-pg.yaml
