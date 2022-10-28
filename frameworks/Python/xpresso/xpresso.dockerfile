FROM python:3.10

RUN mkdir /xpresso
WORKDIR /xpresso

COPY ./requirements.txt /xpresso/

RUN pip install --no-cache-dir -r /xpresso/requirements.txt

COPY ./ /xpresso/

EXPOSE 8080

CMD ["python", "main.py"]
