FROM python:3.14-bookworm

WORKDIR /fastapi

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN apt-get install -y --no-install-recommends libpq-dev
RUN rm -rf /var/lib/apt/lists/*

COPY . ./

RUN pip install -r requirements-fastpysgi.txt

EXPOSE 8080

CMD python app_server.py -s fastpysgi
