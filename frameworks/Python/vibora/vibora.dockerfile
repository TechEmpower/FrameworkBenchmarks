FROM python:3.10.6

ADD ./ /vibora

WORKDIR /vibora

RUN pip3 install -r /vibora/requirements.txt

EXPOSE 8000

CMD ["python3", "app.py"]
