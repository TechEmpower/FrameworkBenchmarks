FROM python:3.6.6-stretch

ADD ./ /vibora

WORKDIR /vibora

RUN pip3 install -r /vibora/requirements.txt

CMD ["python3", "app.py"]
