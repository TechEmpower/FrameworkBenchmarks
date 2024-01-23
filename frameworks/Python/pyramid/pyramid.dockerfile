FROM python:3.10

ADD ./requirements.txt /pyramid/requirements.txt

# https://github.com/mopemope/meinheld/pull/123
RUN pip3 install --no-deps "meinheld==1.0.2"
RUN pip3 install -r /pyramid/requirements.txt
ADD ./ /pyramid
WORKDIR /pyramid

EXPOSE 8080

CMD gunicorn wsgi:app -c gunicorn_conf.py
