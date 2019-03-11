FROM golang:1.12

ENV GO111MODULE on
WORKDIR /iris

COPY src/. .

RUN go get github.com/kataras/iris
RUN go get github.com/lib/pq
RUN go get github.com/valyala/quicktemplate
RUN go get github.com/valyala/quicktemplate/qtc
RUN go mod download

RUN go generate ./templates
RUN go build -ldflags="-s -w" -o app .

CMD ./app -prefork
