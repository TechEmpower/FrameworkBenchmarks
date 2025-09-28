FROM docker.io/golang:1.25.0

WORKDIR /go-faster
COPY ./src /go-faster
RUN make build

EXPOSE 8080
CMD ["./go-faster-techempower"]
