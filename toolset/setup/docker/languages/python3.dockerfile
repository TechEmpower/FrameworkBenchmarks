FROM techempower/base:0.2
  
ENV PY3_ROOT=/py3
ENV PYTHONHOME=${PY3_ROOT}
ENV PY3_VERSION=3.6.4
ENV PATH=${PY3_ROOT}/bin:${PATH}

RUN mkdir py3

RUN wget -q http://www.python.org/ftp/python/${PY3_VERSION}/Python-${PY3_VERSION}.tar.xz
RUN tar xf Python-${PY3_VERSION}.tar.xz

WORKDIR Python-${PY3_VERSION}

RUN ./configure --prefix=$PY3_ROOT --disable-shared --with-computed-gotos --quiet
RUN make -j4 --quiet 2>&1 | tee python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
RUN make install --quiet 2>&1 | tee -a python3-install.log | awk '{ if (NR%100 == 0) printf "."}'

WORKDIR /py3


RUN python3 -m ensurepip -U
RUN pip3 install -U setuptools pip wheel
