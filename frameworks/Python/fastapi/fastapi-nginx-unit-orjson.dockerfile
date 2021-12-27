FROM nginx/unit:1.26.1-python3.9

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==0.29.13

COPY requirements.txt requirements-orjson.txt ./

RUN pip3 install -r requirements.txt -r requirements-orjson.txt

COPY . ./

COPY ./nginx-unit-config-orjson.sh /docker-entrypoint.d/

EXPOSE 8080
