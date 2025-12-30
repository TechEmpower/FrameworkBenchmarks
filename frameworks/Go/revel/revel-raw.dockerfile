FROM golang:1.24.2

ADD ./ /revel
WORKDIR /revel

ENV GOPATH /revel
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/revel/cmd/revel
RUN revel build -a benchmark -t target -m prod -v

EXPOSE 8080

CMD target/run.sh
