FROM python:3.6.6-stretch

ADD ./ /sanic

WORKDIR /sanic

RUN pip3 install -r /sanic/requirements.txt

CMD python3 app.py
