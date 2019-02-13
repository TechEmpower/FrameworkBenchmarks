FROM golang:1.11.5

WORKDIR /go/src/app

RUN go get github.com/kataras/iris
RUN go get github.com/lib/pq
COPY src/. .

RUN go build -ldflags="-s -w" -o app .

CMD ./app
