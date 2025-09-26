FROM python:3.10
EXPOSE 8080
WORKDIR /starlite
COPY . .
RUN pip install --upgrade pip \
    && pip install cython==0.29.26 \
    && pip install -r /starlite/requirements.txt
CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c starlite_conf.py
