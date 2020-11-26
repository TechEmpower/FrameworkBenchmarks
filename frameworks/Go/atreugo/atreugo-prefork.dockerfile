FROM golang:1.15

WORKDIR /atreugo

COPY ./src /atreugo

RUN go get github.com/valyala/quicktemplate/qtc

RUN go generate ./templates
RUN go build -ldflags="-s -w" -o app .

CMD ./app -prefork
