FROM python:3.9-slim


ADD ./requirements.txt /litespeed/requirements.txt
RUN pip3 install -r /litespeed/requirements.txt
ADD ./ /litespeed
WORKDIR /litespeed

EXPOSE 8000

CMD python app.py
