FROM golang:1.15

WORKDIR /fasthttp

COPY ./src /fasthttp

RUN go get github.com/valyala/quicktemplate/qtc

RUN go generate ./templates
RUN go build -ldflags="-s -w" -o app .

CMD ./app
