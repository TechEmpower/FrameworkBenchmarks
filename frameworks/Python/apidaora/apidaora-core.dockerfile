FROM python:3.8-rc-buster

ADD templates/fortune.html /apidaora/templates/

ADD Cython-3.0a0-py3.8-linux-x86_64.egg \
    uvloop-0.14.0.dev0-py3.8-linux-x86_64.egg \
    /apidaora/

WORKDIR /apidaora

RUN easy_install Cython-3.0a0-py3.8-linux-x86_64.egg \
    uvloop-0.14.0.dev0-py3.8-linux-x86_64.egg

ADD requirements.txt /apidaora/

RUN pip3 install -r /apidaora/requirements.txt

ADD apidaora_core_conf.py coreapp.py /apidaora/

CMD gunicorn coreapp:app -k uvicorn.workers.UvicornWorker -c apidaora_core_conf.py
