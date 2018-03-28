FROM tfb/python2:latest

ADD ./ /django

WORKDIR /django

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /django/requirements.txt
