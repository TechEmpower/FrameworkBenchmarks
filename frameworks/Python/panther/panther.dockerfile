FROM python:3.12 AS builder
WORKDIR /panther
RUN python -m venv /opt/venv
RUN pip install --no-cache-dir uv
COPY requirements.txt .
RUN /usr/local/bin/uv pip install -r requirements.txt --python /opt/venv/bin/python

FROM python:3.12-slim AS production
ENV PYTHONUNBUFFERED=1
ENV PATH="/opt/venv/bin:$PATH"
COPY --from=builder /opt/venv /opt/venv
WORKDIR /panther
COPY . /panther

EXPOSE 8080
CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c panther_conf.py
