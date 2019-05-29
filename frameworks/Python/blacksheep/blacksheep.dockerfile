FROM python:3.7.2-stretch

ADD ./ /blacksheep

WORKDIR /blacksheep

RUN pip3 install -r /blacksheep/requirements.txt

CMD python app.py -O
