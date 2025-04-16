FROM python:3.13-slim

WORKDIR /blacksheep

COPY ./ /blacksheep

RUN pip3 install -U pip
RUN pip3 install Cython==3.0.12
RUN pip3 install -r /blacksheep/requirements.txt
RUN pip3 install -r /blacksheep/requirements-gunicorn.txt
RUN pip3 install -r /blacksheep/requirements-uvicorn.txt

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c blacksheep_conf.py
