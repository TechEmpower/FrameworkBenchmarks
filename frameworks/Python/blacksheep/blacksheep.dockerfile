FROM python:3.13

WORKDIR /blacksheep

COPY ./ /blacksheep

RUN pip3 install -U pip -q
RUN pip3 install Cython==3.0.12 -q
RUN pip3 install -r /blacksheep/requirements.txt -q
RUN pip3 install -r /blacksheep/requirements-gunicorn.txt -q
RUN pip3 install -r /blacksheep/requirements-uvicorn.txt -q
ENV GUNICORN=1
EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c blacksheep_conf.py
