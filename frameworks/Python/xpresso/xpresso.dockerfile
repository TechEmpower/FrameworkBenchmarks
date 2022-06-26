FROM python:3.10-slim

ADD ./ /xpresso

WORKDIR /xpresso

RUN pip install --no-cache-dir -r /xpresso/requirements.txt

EXPOSE 8080

CMD ["python", "app.py"]
