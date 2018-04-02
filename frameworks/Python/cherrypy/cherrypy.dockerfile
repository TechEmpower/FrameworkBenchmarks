FROM techempower/python2:0.1

ADD ./ /cherrypy

WORKDIR /cherrypy

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /cherrypy/requirements.txt

CMD python app.py
