FROM python:3.11-bullseye

WORKDIR /usr/src/app

COPY requirements.txt ./
RUN apt-get update
RUN pip install --no-cache-dir ujson
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 3000

CMD python ./app.py
