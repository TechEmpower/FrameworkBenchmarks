FROM python:3.8

ADD ./requirements.txt /sanic/requirements.txt

RUN pip3 install cython==0.29.13 && \
    pip3 install -r /sanic/requirements.txt

ADD ./ /sanic

WORKDIR /sanic

EXPOSE 8080

CMD python3 app.py
