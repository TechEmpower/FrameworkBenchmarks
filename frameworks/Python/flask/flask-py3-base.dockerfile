FROM tfb/python3:latest

ADD ./ /flask

WORKDIR /flask

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /flask/requirements.txt
