FROM python:3.8

ADD templates/fortune.html /apidaora/templates/

WORKDIR /apidaora

ADD requirements.txt /apidaora/

RUN pip3 install cython==0.29.13

RUN pip3 install -r /apidaora/requirements.txt

ADD apidaora_conf.py app.py /apidaora/

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c apidaora_conf.py
