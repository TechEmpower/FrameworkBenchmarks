FROM golang:1.16

WORKDIR /fasthttp

COPY ./src /fasthttp

RUN go get -u github.com/valyala/quicktemplate/qtc

RUN go generate ./templates
RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -prefork
