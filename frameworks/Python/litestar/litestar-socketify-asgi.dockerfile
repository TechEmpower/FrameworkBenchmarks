FROM python:3.13-bullseye

WORKDIR /litestar

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN apt-get update; apt-get install libuv1 -y
RUN pip3 install cython==3.0.12

COPY requirements-socketify.txt ./

RUN pip3 install -r requirements-socketify.txt

COPY . ./

EXPOSE 8080

CMD python ./app-socketify-asgi.py
