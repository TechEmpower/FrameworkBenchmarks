FROM python:3.11-bullseye

WORKDIR /usr/src/app

COPY requirements.txt ./
RUN pip3 install -U pip
RUN pip3 install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 3000

CMD python ./app-asgi.py
