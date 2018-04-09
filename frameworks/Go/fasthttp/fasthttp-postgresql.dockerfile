FROM golang:1.10.1

ADD ./ /fasthttp
WORKDIR /fasthttp

RUN mkdir bin
ENV GOPATH /fasthttp
ENV PATH ${GOPATH}/bin:${PATH}

RUN rm -rf ./pkg/*
RUN go get -d -u github.com/jackc/pgx
RUN go get -d -u github.com/valyala/fasthttp/...
RUN go get -u github.com/valyala/quicktemplate/qtc
RUN go get -u github.com/mailru/easyjson/...

RUN rm -f ./server-postgresql
RUN go generate templates
RUN go build -gcflags='-l=4' server-postgresql

CMD ./server-postgresql
