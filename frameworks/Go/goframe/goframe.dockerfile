FROM golang:1.14

ADD ./ /goframe
WORKDIR /goframe

RUN mkdir  bin
ENV GOPATH /goframe
ENV PATH   ${GOPATH}/bin:${PATH}

RUN go get -u -v github.com/gogf/gf
RUN go get -u -v github.com/go-sql-driver/mysql

RUN go build -o main main.go

CMD ./main
