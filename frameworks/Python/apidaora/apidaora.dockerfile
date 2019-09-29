FROM python:3.8-rc-buster

ADD templates/fortune.html /apidaora/templates/

ADD Cython-3.0a0-py3.8-linux-x86_64.egg \
    orjson-2.0.7-cp38-cp38-manylinux1_x86_64.whl \
    uvloop-0.14.0.dev0-py3.8-linux-x86_64.egg \
    /apidaora/

WORKDIR /apidaora

RUN easy_install Cython-3.0a0-py3.8-linux-x86_64.egg \
    uvloop-0.14.0.dev0-py3.8-linux-x86_64.egg && \
    pip3 install orjson-2.0.7-cp38-cp38-manylinux1_x86_64.whl

ADD requirements.txt /apidaora/

RUN pip3 install -r /apidaora/requirements.txt

ADD apidaora_conf.py app.py /apidaora/

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c apidaora_conf.py
