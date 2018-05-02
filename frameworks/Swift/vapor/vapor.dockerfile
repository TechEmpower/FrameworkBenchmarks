FROM codevapor/swift:4.1

ADD ./ /vapor
WORKDIR /vapor
# RUN curl -sL -o apt.vapor.sh https://apt.vapor.sh
# RUN bash apt.vapor.sh
# RUN apt update -yqq
RUN swift build -c release

CMD .build/release/vapor-tfb --env production
