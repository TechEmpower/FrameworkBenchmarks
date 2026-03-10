FROM python:3.14

WORKDIR /fastapi

RUN apt update
RUN apt install nginx -y
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.2.3

COPY . ./

RUN pip3 install -r requirements-gunicorn.txt

ENV PGSSLMODE disable

EXPOSE 8080

RUN chmod +x /fastapi/nginx-entrypoint.sh
ENTRYPOINT ["/fastapi/nginx-entrypoint.sh"]
