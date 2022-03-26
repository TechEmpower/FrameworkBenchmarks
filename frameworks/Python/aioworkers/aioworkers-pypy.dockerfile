FROM pypy:3.8-bullseye

ADD ./requirements.txt /aioworkers/

WORKDIR /aioworkers

RUN pip3 install -U pip && \
    pip3 install -r /aioworkers/requirements.txt

ADD ./ /aioworkers

EXPOSE 8080

CMD aioworkers aioworkers.net.web --multiprocessing -c config.yaml
