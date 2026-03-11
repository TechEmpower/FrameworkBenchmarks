FROM pypy:3.11-bookworm

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN apt-get update; apt-get install libuv1 -y

COPY . ./

RUN pip3 install -r requirements-socketify-pypy.txt

EXPOSE 8080

CMD python app_server.py -s socketify
