FROM nginx/unit:1.29.1-python3.11

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==0.29.33

COPY requirements.txt ./

RUN pip3 install -r requirements.txt

COPY . ./

COPY ./nginx-unit-config.sh /docker-entrypoint.d/

ENV PGSSLMODE disable

EXPOSE 8080
