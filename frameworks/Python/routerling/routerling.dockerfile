FROM python:3.8

WORKDIR /routerling

RUN pip3 install cython==0.29.13

ADD requirements.txt /routerling/

RUN pip3 install -r /routerling/requirements.txt

ADD templates/fortune.html /routerling/templates/

ADD settings.py app.py /routerling/

EXPOSE 8080

CMD gunicorn app:router -k uvicorn.workers.UvicornWorker -c settings.py