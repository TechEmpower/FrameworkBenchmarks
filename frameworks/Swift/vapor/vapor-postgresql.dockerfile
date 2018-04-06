FROM swift:4.1

ADD ./ /vapor
WORKDIR /vapor
RUN curl -sL -o apt.vapor.sh https://apt.vapor.sh
RUN bash apt.vapor.sh
RUN apt update -yqq && apt install -yqq ctls cmysql
RUN swift build -Xswiftc -DNOJSON -c release

CMD .build/release/vapor-tfb-postgresql --env=production
