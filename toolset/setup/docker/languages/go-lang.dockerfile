FROM techempower/base:0.1

RUN mkdir /go-lang
WORKDIR /go-lang
RUN curl -sL https://storage.googleapis.com/golang/go1.10.linux-amd64.tar.gz | tar xz

ENV GOROOT /go-lang/go
ENV PATH ${GOROOT}/bin:${PATH}

ENV GOGC 1000
