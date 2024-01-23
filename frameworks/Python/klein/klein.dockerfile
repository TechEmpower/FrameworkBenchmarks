FROM python:2.7.15-stretch

ADD ./ /klein

WORKDIR /klein

RUN pip install -r /klein/requirements.txt

EXPOSE 8080

CMD python app.py
