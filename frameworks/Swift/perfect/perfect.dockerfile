FROM swift:4.1

ADD . /perfect
WORKDIR /perfect
RUN apt update -yqq && apt install -yqq libpq-dev && apt install -y xsltproc docbook-xsl uuid-dev
RUN swift build
CMD .build/debug/PerfectTemplate