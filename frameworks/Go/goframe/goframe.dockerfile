FROM golang:1.17

ADD     ./src /goframe
WORKDIR /goframe
RUN     go get -u github.com/valyala/quicktemplate/qtc
RUN     go generate ./template
RUN     go build -o main main.go

EXPOSE 8080

CMD ./main
