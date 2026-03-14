FROM python:3.14

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.2.3

COPY . ./

RUN pip3 install -r requirements-sqlalchemy.txt

EXPOSE 8080

ENV CONNECTION=ORM

CMD gunicorn app_orm:app -k uvicorn.workers.UvicornWorker -c fastapi_conf.py
