FROM python:2.7.15-stretch

ADD ./ /klein

WORKDIR /klein

RUN pip install -r /klein/requirements.txt

CMD python app.py
