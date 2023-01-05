FROM pypy:3.9-bullseye

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

COPY requirements-socketify-pypy.txt ./
RUN apt-get update; apt-get install libuv1 -y
RUN pip3 install -r requirements-socketify-pypy.txt

COPY . ./

EXPOSE 8080

CMD python ./app-socketify-asgi.py