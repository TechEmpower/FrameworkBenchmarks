FROM python:3.13

WORKDIR /litestar

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.0.12

COPY requirements.txt requirements-granian.txt ./

RUN pip3 install -r requirements.txt -r requirements-granian.txt

COPY . ./

EXPOSE 8080

CMD granian --interface asgi app:app --host '0.0.0.0' --port 8080 --workers $(nproc)
