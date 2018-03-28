FROM tfb/python3:latest

ADD ./ /sanic

WORKDIR /sanic

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /sanic/requirements.txt

CMD python3 app.py
