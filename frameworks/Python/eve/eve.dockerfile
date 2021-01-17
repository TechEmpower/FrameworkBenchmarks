FROM aleksxp/docker-eve-python:legacy

ADD ./ /eve

WORKDIR /eve

COPY . .

RUN pip install -r requirements.txt

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
