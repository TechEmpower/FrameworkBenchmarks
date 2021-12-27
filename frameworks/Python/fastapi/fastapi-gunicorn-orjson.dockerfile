FROM python:3.9

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==0.29.13

COPY requirements.txt requirements-orjson.txt requirements-gunicorn.txt ./

RUN pip3 install -r requirements.txt -r requirements-orjson.txt -r requirements-gunicorn.txt

COPY . ./

EXPOSE 8080

CMD gunicorn app_orjson:app -k uvicorn.workers.UvicornWorker -c fastapi_conf.py