FROM golang:1.15

ADD     ./src /goframe
WORKDIR /goframe
RUN     go build -o main main.go
CMD     ./main
