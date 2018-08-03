FROM python:3.6.5

ADD ./ /cherrypy

WORKDIR /cherrypy

RUN pip3 install -r /cherrypy/requirements.txt

CMD python3 app.py
