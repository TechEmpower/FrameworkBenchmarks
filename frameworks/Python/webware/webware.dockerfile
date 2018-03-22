FROM tfb/python2:latest

ADD ./ /webware

WORKDIR /webware

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /webware/requirements.txt

RUN wget -q https://downloads.sourceforge.net/webware/Webware-1.1.1.tar.gz
RUN tar xf Webware-1.1.1.tar.gz
RUN cp -r app/ Webware-1.1.1/

WORKDIR /webware/Webware-1.1.1

RUN python install.py --no-password-prompt

WORKDIR /webware/Webware-1.1.1/app

CMD python Launch.py
