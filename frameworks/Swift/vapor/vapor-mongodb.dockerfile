FROM swift:4.1

ADD ./ /vapor
WORKDIR /vapor
RUN curl -sL -o apt.vapor.sh https://apt.vapor.sh
RUN bash apt.vapor.sh
RUN apt update -yqq
RUN apt install -yqq ctls cmysql libmysqlclient-dev libpq-dev > /dev/null
RUN swift build -Xswiftc -DNOJSON -c release

CMD .build/release/vapor-tfb-mongodb --env=production
