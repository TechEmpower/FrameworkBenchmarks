FROM tfb/swift:latest
ADD ./ /vapor
WORKDIR /vapor
RUN curl -sL -o apt.vapor.sh https://apt.vapor.sh
RUN bash apt.vapor.sh
RUN apt-get install -y ctls cmysql
RUN swift build -Xswiftc -DNOJSON -c release
