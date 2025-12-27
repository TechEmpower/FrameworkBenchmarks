FROM python:3.14

WORKDIR /fastapi

RUN apt update && \
    apt install nginx -y
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip3 install cython==3.2.3
COPY requirements.txt requirements-gunicorn.txt requirements-uvicorn.txt ./
RUN pip3 install -r requirements.txt -r requirements-gunicorn.txt -r requirements-uvicorn.txt

COPY . ./
ENV PGSSLMODE disable

EXPOSE 8080

RUN chmod +x /fastapi/nginx-entrypoint.sh
ENTRYPOINT ["/fastapi/nginx-entrypoint.sh"]