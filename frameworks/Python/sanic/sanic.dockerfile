FROM python:3.6.6-stretch

ADD ./requirements.txt /sanic/requirements.txt

RUN pip3 install -r /sanic/requirements.txt

ADD ./ /sanic

WORKDIR /sanic

CMD python3 app.py
