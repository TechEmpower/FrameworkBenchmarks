FROM golang:1.16

ENV GO111MODULE on
WORKDIR /go-std

COPY ./src /go-std

RUN go get github.com/valyala/quicktemplate/qtc
RUN go get -u github.com/mailru/easyjson/...
RUN go mod download

# Start Contrast Additions
COPY contrast-go contrast-go
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=true
ENV CONTRAST__PROTECT__ENABLE=false

ENV CONTRAST__AGENT__SERVICE__BYPASS=true

RUN chmod 777 ./contrast-go
# End Contrast Additions

RUN go generate ./templates
RUN easyjson -pkg
RUN ./contrast-go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -db pgx
