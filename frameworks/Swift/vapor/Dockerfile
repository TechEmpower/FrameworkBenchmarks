FROM swiftdocker/swift:latest

ENV PATH /usr/bin:$PATH

RUN mkdir -p /vapor
WORKDIR /vapor
ADD . /vapor
RUN ./build

EXPOSE 8080

CMD .build/debug/App
