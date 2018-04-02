FROM techempower/python2:0.1

ADD ./ /klein

WORKDIR /klein

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /klein/requirements.txt

CMD python app.py
