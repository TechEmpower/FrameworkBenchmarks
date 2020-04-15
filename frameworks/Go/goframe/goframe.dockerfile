FROM golang:1.14

ADD ./src /goframe
ENV GO111MODULE on
RUN go build -o main main.go
CMD ./goframe/main
