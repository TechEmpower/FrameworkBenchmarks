FROM nginx/unit:1.29.1-python3.13

WORKDIR /litestar

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.0.12

COPY requirements.txt ./

RUN pip3 install -r requirements.txt

COPY . ./

COPY ./nginx-unit-config.sh /docker-entrypoint.d/

ENV PGSSLMODE disable

EXPOSE 8080
