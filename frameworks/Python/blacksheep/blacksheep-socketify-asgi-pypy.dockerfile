FROM pypy:3.11-bookworm

ADD ./ /blacksheep

WORKDIR /blacksheep

RUN apt-get update; apt-get install libuv1 libpq5 -y

RUN pip3 install -r /blacksheep/requirements.txt
RUN pip3 install -r /blacksheep/requirements-pypy.txt

EXPOSE 8080

CMD python ./app.py

