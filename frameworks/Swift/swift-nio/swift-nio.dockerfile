FROM swift:4.2

ADD ./ /swift-nio
WORKDIR /swift-nio
RUN swift build -c release

CMD .build/release/swift-nio-tfb-default
