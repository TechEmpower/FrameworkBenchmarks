FROM tfb/python2:latest

ADD ./ /cherrypy

WORKDIR /cherrypy

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /cherrypy/requirements.txt

CMD python app.py
