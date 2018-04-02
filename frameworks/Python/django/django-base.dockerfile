FROM techempower/python2:0.1

ADD ./ /django

WORKDIR /django

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /django/requirements.txt
