FROM mmicael/nginx-unit:master-python3.11

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==0.29.32

COPY requirements.txt requirements-orjson.txt ./

RUN pip3 install -r requirements.txt -r requirements-orjson.txt

COPY . ./

COPY ./nginx-unit-config-orjson.sh /docker-entrypoint.d/

ENV PGSSLMODE disable

EXPOSE 8080
