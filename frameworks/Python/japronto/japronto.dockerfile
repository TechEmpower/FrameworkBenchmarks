FROM tfb/python3:latest

ADD ./ /japronto

WORKDIR /japronto

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /japronto/requirements.txt

CMD python3 app.py
