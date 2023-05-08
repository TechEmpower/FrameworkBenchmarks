FROM python:3.11-bullseye

WORKDIR /blacksheep

COPY ./ /blacksheep

RUN pip3 install -U pip
RUN pip3 install cython==0.29.34
RUN pip3 install -r /blacksheep/requirements.txt
RUN pip3 install -r /blacksheep/requirements-gunicorn.txt
RUN pip3 install -r /blacksheep/requirements-uvicorn.txt

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c blacksheep_conf.py
