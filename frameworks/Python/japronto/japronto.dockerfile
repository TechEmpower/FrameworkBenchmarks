FROM python:3.6.5

ADD ./ /japronto

WORKDIR /japronto

RUN pip3 install -r /japronto/requirements.txt

CMD python3 app.py
