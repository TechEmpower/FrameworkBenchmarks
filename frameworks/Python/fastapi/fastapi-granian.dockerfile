FROM python:3.11

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

COPY requirements.txt requirements-granian.txt ./

RUN pip3 install \
    -r requirements.txt \
    -r requirements-granian.txt

COPY . ./

EXPOSE 8080

CMD granian \
    --host=0.0.0.0 \
    --port=8080 \
    --interface=asgi \
    --loop=uvloop  \
    --workers=$(nproc) \
    --log-level=error \
    app:app
