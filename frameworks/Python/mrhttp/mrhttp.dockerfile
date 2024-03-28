
FROM python:3.8.12

ADD ./ /mrhttp

WORKDIR /mrhttp

RUN pip3 install -r /mrhttp/requirements.txt

EXPOSE 8080

CMD python3 app.py

