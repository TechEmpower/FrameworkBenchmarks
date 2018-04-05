FROM techempower/base:0.1

RUN apt-get install -y clang

RUN mkdir /swift
WORKDIR /swift
RUN curl -sL https://swift.org/builds/swift-4.0.3-release/ubuntu1604/swift-4.0.3-RELEASE/swift-4.0.3-RELEASE-ubuntu16.04.tar.gz | tar xz

ENV SWIFT_HOME /swift/swift-4.0.3-RELEASE-ubuntu16.04
ENV PATH ${SWIFT_HOME}/usr/bin:${PATH}
