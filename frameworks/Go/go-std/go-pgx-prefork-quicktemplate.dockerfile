FROM docker.io/golang:1.19

WORKDIR /go-std

COPY ./src /go-std

RUN go mod download

# next line intentionnaly commented to prevent generating easyjson code:
# RUN go generate -x ./...

# generate only quicktempalte code:
RUN go generate -x ./templates

RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -db pgx -prefork
