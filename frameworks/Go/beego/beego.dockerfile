FROM golang:1.14

ADD ./ /beego
WORKDIR /beego

RUN mkdir bin
ENV GOPATH /beego
ENV PATH ${GOPATH}/bin:${PATH}

RUN curl -sL -o install_glide.sh https://raw.githubusercontent.com/Masterminds/glide.sh/master/get
RUN sh install_glide.sh

RUN glide -v
WORKDIR src/hello
RUN glide install

RUN go build -o server main.go

EXPOSE 8080

CMD ./server

