FROM golang:1.14

ADD ./ /revel
WORKDIR /revel

ENV GOPATH /revel
ENV PATH ${GOPATH}/bin:${PATH}


RUN go get -u github.com/revel/cmd/revel
RUN revel build -a benchmark -t target -m prod -v

CMD target/run.sh

