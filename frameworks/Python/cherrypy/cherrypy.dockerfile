FROM python:2.7.14

ADD ./ /cherrypy

WORKDIR /cherrypy

RUN pip install -r /cherrypy/requirements.txt

CMD python app.py
