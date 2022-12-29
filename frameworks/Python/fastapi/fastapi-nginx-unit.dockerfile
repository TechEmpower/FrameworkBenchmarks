FROM nginx/unit:1.28.0-python3.10

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==0.29.32

COPY requirements.txt ./

RUN pip3 install -r requirements.txt

COPY . ./

COPY ./nginx-unit-config.sh /docker-entrypoint.d/

ENV PGSSLMODE disable

EXPOSE 8080
