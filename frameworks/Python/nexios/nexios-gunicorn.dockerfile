FROM python:3.13

WORKDIR /nexios

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.0.12

COPY requirements.txt requirements-gunicorn.txt ./

RUN pip3 install -r requirements.txt -r requirements-gunicorn.txt

COPY . ./

EXPOSE 8080

CMD gunicorn app:app --bind 0.0.0.0:8080 --workers $(nproc) --worker-class uvicorn.workers.UvicornWorker --log-level error --access-logfile - --error-logfile - 