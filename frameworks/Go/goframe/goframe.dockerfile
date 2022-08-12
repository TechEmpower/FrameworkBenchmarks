FROM golang:1.17

ADD     ./src /goframe
WORKDIR /goframe
RUN     go get -u github.com/valyala/quicktemplate/qtc@v1.6.3
RUN     go mod tidy
RUN     go generate ./template
RUN     go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
