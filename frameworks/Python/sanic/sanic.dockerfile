FROM python:3.12

ADD ./requirements.txt /sanic/requirements.txt

RUN pip3 install cython==3.0.11 && \
    pip3 install -r /sanic/requirements.txt

ADD ./ /sanic

WORKDIR /sanic

EXPOSE 8080

CMD python3 app.py
