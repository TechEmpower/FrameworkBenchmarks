FROM python:3.11

WORKDIR /heaven

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==0.29.36

COPY requirements.txt ./

RUN pip3 install -r requirements.txt 

COPY . ./

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c heaven_conf.py
