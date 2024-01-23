FROM python:2.7.15-stretch

ADD ./ /cherrypy

WORKDIR /cherrypy

RUN pip install -r /cherrypy/requirements.txt

EXPOSE 8080

CMD python app.py
