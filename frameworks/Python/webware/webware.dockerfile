FROM python:2.7.15-stretch

ADD ./ /webware

WORKDIR /webware

RUN pip install -r /webware/requirements.txt

RUN wget -q https://downloads.sourceforge.net/webware/Webware-1.1.1.tar.gz
RUN tar xf Webware-1.1.1.tar.gz
RUN cp -r app/ Webware-1.1.1/

WORKDIR /webware/Webware-1.1.1

RUN python install.py --no-password-prompt

WORKDIR /webware/Webware-1.1.1/app

EXPOSE 8080

CMD python Launch.py
