FROM golang:1.11.5

WORKDIR /go/src/app

RUN go get github.com/kataras/iris
RUN go get github.com/lib/pq
RUN go get github.com/valyala/quicktemplate
RUN go get -u github.com/valyala/quicktemplate/qtc
COPY src/. .

RUN qtc
RUN go build -ldflags="-s -w" -o app .

CMD ./app
