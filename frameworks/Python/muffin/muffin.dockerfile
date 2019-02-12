FROM python:3.6.6-stretch

ADD ./ /muffin

WORKDIR /muffin

RUN pip3 install -r /muffin/requirements.txt

# EXPOSE 5000

# CMD ["gunicorn", "app:app", "-c", "gunicorn_conf.py"]
# CMD ["muffin", "app:app", "run"]
# CMD ["gunicorn", "-w 4", "app:app", "gunicorn_conf.py"]
# CMD ["gunicorn", "-w", "23", "app:app", "-c", "gunicorn_conf.py"]
CMD ["gunicorn", "-w", "4", "app:app", "-c", "gunicorn_conf.py"]