FROM techempower/go-lang:0.1

ADD ./ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}
