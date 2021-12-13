FROM python:3.8

WORKDIR /blacksheep

RUN pip3 install cython==0.29.13

ADD requirements.txt /blacksheep/

RUN pip3 install -r /blacksheep/requirements.txt

ADD templates/fortune.html /blacksheep/templates/

ADD blacksheep_conf.py app.py /blacksheep/

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c blacksheep_conf.py
