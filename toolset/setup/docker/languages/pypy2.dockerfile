FROM techempower/base:0.1
  
ENV PYPY2_ROOT=/pypy2
ENV PYTHONHOME=${PYPY2_ROOT}
ENV PYPY2_VERSION=5.10.0
ENV PATH=${PYPY2_ROOT}/bin:${PATH}

RUN wget -q https://bitbucket.org/pypy/pypy/downloads/pypy2-v${PYPY2_VERSION}-linux64.tar.bz2
RUN tar xf pypy2-v${PYPY2_VERSION}-linux64.tar.bz2
RUN mv pypy2-v${PYPY2_VERSION}-linux64 $PYPY2_ROOT

RUN pypy -m ensurepip
RUN pip install -U pip setuptools wheel
