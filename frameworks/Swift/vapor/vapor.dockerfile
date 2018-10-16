FROM swift:4.2

ADD ./ /vapor
WORKDIR /vapor
RUN swift build -c release

CMD .build/release/Run -e production -b 0.0.0.0:8080
