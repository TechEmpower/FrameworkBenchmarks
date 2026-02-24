FROM python:3.13-slim

ADD ./ /app

WORKDIR /app

RUN pip install -r /app/requirements.txt

EXPOSE 8080

CMD ["pyvoy", "--interface", "wsgi", "app_wsgi:main", "--address=0.0.0.0", "--port", "8080"]