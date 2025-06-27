FROM python:3.13

WORKDIR /litestar

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.0.12

COPY requirements.txt requirements-sqlalchemy.txt requirements-gunicorn.txt ./

RUN pip3 install -r requirements.txt -r requirements-sqlalchemy.txt -r requirements-gunicorn.txt

COPY . ./

EXPOSE 8080

ENV CONNECTION=ORM

CMD gunicorn app_orm:app -k uvicorn_worker.UvicornWorker -c litestar_conf.py
