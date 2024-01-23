FROM python:3.6.6-stretch

WORKDIR /hug
COPY app.py app.py
COPY uwsgi.ini uwsgi.ini
COPY requirements.txt requirements.txt

RUN pip3 install -r requirements.txt

EXPOSE 8080

CMD ["uwsgi", "--ini", "uwsgi.ini"]
