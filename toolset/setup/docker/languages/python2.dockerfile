FROM techempower/base:0.1
  
ENV PY2_ROOT=/py2
ENV PYTHONHOME=${PY2_ROOT}
ENV PY2_VERSION=2.7.14
ENV PATH=${PY2_ROOT}/bin:${PATH}

RUN mkdir $PY2_ROOT

RUN wget -q http://www.python.org/ftp/python/${PY2_VERSION}/Python-${PY2_VERSION}.tar.xz
RUN tar xf Python-${PY2_VERSION}.tar.xz

WORKDIR Python-${PY2_VERSION}

RUN ./configure --prefix=$PY2_ROOT --disable-shared --with-computed-gotos --quiet
RUN make -j4 --quiet 2>&1 | tee python2-install.log | awk '{ if (NR%100 == 0) printf "."}'
RUN make install --quiet 2>&1 | tee -a python2-install.log | awk '{ if (NR%100 == 0) printf "."}'

WORKDIR /py2

RUN python -m ensurepip -U
RUN pip install -U setuptools pip wheel
