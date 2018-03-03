FROM tfb/base:latest

RUN apt install -yqq llvm-dev libjemalloc-dev zlib1g-dev libssl-dev libcurl4-openssl-dev bison

ENV RUBY_VERSION 2.4.2
ENV RUBY_GEMS 2.6.12

RUN cd /usr/local/src && \
    wget https://cache.ruby-lang.org/pub/ruby/2.4/ruby-${RUBY_VERSION}.tar.gz && \
    tar zxvf ruby-${RUBY_VERSION}.tar.gz && \
    cd ruby-${RUBY_VERSION} && \
    ./configure && \
    make && \
    make install

RUN wget https://rubygems.org/rubygems/rubygems-${RUBY_GEMS}.tgz && \
    tar zxvf rubygems-${RUBY_GEMS}.tgz && \
    cd rubygems-${RUBY_GEMS} && \
    /usr/local/bin/ruby setup.rb

RUN gem install bundler --no-ri --no-rdoc
