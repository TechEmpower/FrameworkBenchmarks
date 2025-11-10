FROM python:3.13

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.0.12

COPY requirements.txt requirements-orjson.txt requirements-gunicorn.txt requirements-uvicorn.txt ./

RUN pip3 install -r requirements.txt -r requirements-orjson.txt -r requirements-gunicorn.txt -r requirements-uvicorn.txt

COPY . ./

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c fastapi_conf.py
