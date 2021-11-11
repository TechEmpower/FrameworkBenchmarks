FROM python:3.6.6-stretch

ADD ./ /tornado

WORKDIR /tornado

RUN pip3 install -r /tornado/requirements_py3_uvloop.txt

EXPOSE 8080

CMD python3 server_py3_uvloop.py --logging=error
