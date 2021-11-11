FROM python:3.6.6-stretch

ADD ./yocto_http /yocto_http
ADD ./requirements.txt /yocto_http

WORKDIR /yocto_http

RUN pip3 install -r /yocto_http/requirements.txt

WORKDIR /yocto_http

EXPOSE 8082

CMD api_hour -ac hello:Container
