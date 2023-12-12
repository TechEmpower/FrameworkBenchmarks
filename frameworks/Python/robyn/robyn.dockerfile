FROM python:3.12

ADD ./ /robyn

WORKDIR /robyn

RUN pip3 install -r /robyn/requirements.txt

EXPOSE 8080

CMD ["python", "app.py", "--log-level", "warn"]
