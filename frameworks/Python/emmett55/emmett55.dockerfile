FROM python:3.13-slim

ADD ./ /emmett55

WORKDIR /emmett55

RUN pip install --no-cache-dir -r /emmett55/requirements.txt

EXPOSE 8080

CMD python run.py
