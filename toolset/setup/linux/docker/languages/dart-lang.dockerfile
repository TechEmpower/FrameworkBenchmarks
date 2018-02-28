FROM tfb/base:latest

# Dart version 1.10.0
RUN wget http://storage.googleapis.com/dart-archive/channels/stable/release/45396/sdk/dartsdk-linux-x64-release.zip
RUN unzip dartsdk-linux-x64-release.zip

ENV PUB_CACHE=/.pubcache
ENV PATH=dart-sdk/bin:${PATH}
