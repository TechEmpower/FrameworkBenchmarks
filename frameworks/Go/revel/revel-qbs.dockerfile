FROM golang:1.10.1

ADD ./ /revel
WORKDIR /revel

RUN mkdir bin
ENV GOPATH /revel
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get -u github.com/go-sql-driver/mysql
RUN go get -u github.com/revel/cmd/revel
RUN go get -u github.com/coocood/qbs
RUN go get -u github.com/eaigner/jet

CMD revel run benchmark prod
