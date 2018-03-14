FROM tfb/pypy2:latest

ADD ./ /flask

WORKDIR /flask

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /flask/requirements-pypy.txt
