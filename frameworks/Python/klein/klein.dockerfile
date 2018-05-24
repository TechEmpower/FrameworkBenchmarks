FROM python:2.7.14

ADD ./ /klein

WORKDIR /klein

RUN pip install -r /klein/requirements.txt

CMD python app.py
