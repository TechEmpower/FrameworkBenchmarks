FROM golang:1.18

ENV GO111MODULE=on
WORKDIR /src/
ADD ./hertz-gorm /src/

RUN go mod tidy
#- original submission
RUN go build -o app
#RUN go build -tags=jsoniter -o app - tryed this but slower on my pc

ENTRYPOINT ["/src/app"]
