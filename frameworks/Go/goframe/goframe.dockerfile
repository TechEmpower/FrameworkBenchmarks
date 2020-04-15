FROM golang:1.14

ADD ./src /goframe
RUN cd /goframe && go build -o main main.go
CMD ./goframe/main
