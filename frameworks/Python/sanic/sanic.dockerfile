FROM python:3.14

ADD ./requirements.txt /sanic/requirements.txt

RUN pip3 install cython==3.2.3 && \
    pip3 install -r /sanic/requirements.txt

ADD ./ /sanic

WORKDIR /sanic

EXPOSE 8080

CMD python3 app.py
