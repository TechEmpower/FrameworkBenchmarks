FROM tfb/java:latest

WORKDIR /

RUN wget -q https://repo1.maven.org/maven2/org/jruby/jruby-dist/9.1.16.0/jruby-dist-9.1.16.0-bin.tar.gz
RUN tar xf jruby-dist-9.1.16.0-bin.tar.gz

ENV PATH=/jruby-9.1.16.0/bin:${PATH}

RUN jruby -S gem install bundler -v 1.15.4
