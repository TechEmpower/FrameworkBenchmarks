FROM swift:4.1

ADD ./ /vapor
WORKDIR /vapor
RUN swift build -c release

CMD .build/release/vapor-tfb -e production -b 0.0.0.0:8080
