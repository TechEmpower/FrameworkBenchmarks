FROM techempower/pypy2:0.1

ADD ./ /flask

WORKDIR /flask

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /flask/requirements-pypy.txt
