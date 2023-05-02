FROM python:3.10-slim

ADD ./ /emmett

WORKDIR /emmett

RUN pip install --no-cache-dir -r /emmett/requirements.txt

EXPOSE 8080

CMD python run.py
