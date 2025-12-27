FROM nginx/unit:1.29.1-python3.11

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
RUN apt update && \
    apt install software-properties-common -y && \
    add-apt-repository ppa:deadsnakes/ppa && \
    apt update
RUN apt install python3.14
RUN pip3 install cython==3.2.3

COPY requirements.txt ./

RUN pip3 install -r requirements.txt

COPY . ./

COPY ./nginx-unit-config.sh /docker-entrypoint.d/

ENV PGSSLMODE disable

EXPOSE 8080
