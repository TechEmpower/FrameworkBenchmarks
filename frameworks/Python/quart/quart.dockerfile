FROM python:3.7-stretch

ADD ./ /quart

WORKDIR /quart

RUN pip3 install -r /quart/requirements.txt

CMD python3 app.py
