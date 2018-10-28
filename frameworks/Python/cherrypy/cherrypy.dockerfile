FROM python:2.7.15-stretch

ADD ./ /cherrypy

WORKDIR /cherrypy

RUN pip install -r /cherrypy/requirements.txt

CMD python app.py
