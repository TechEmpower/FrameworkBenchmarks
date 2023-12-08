FROM python:3.11-slim

ADD ./ /granian

WORKDIR /granian

RUN pip install -r /granian/requirements.txt

EXPOSE 8080

CMD python run.py rsgi runtime
