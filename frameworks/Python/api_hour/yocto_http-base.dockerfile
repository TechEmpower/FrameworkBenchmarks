FROM tfb/python3:latest

ADD ./yocto_http /yocto_http
ADD ./requirements.txt /yocto_http

WORKDIR /yocto_http

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /yocto_http/requirements.txt
