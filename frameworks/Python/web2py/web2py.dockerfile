FROM python:2.7.15-stretch

ADD ./ /web2py

WORKDIR /web2py

RUN pip install -r /web2py/requirements.txt

RUN git clone --recursive --branch master https://github.com/web2py/web2py.git

WORKDIR /web2py/web2py

# Version R-2.17.2
RUN git checkout 95709e582d586bd1871ab0fe9f6f265c1884bbe3

WORKDIR /web2py

RUN cp -r app/standard/ web2py/applications/
RUN cp -r app/optimized/ web2py/applications/
RUN cp app/wsgi.py web2py/
RUN cp app/routes.py web2py/
RUN touch web2py/__init__.py
RUN python compile_apps.py

WORKDIR /web2py

EXPOSE 8080

CMD gunicorn web2py.wsgi:application -c gunicorn_conf.py
