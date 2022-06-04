FROM golang:1.14

ADD ./ /revel
WORKDIR /revel

ENV GOPATH /revel
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get -u github.com/revel/cmd/revel
RUN revel build -a benchmark -t target -m prod-fasthttp -v

EXPOSE 8080

CMD target/run.sh
