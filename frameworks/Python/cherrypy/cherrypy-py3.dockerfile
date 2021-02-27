FROM python:3.6.6-stretch

ADD ./ /cherrypy

WORKDIR /cherrypy

RUN pip3 install -r /cherrypy/requirements.txt

EXPOSE 8080

CMD python3 app.py
