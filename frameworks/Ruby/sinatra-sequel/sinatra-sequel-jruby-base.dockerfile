FROM jruby:9.4-jdk17

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

ENV THREAD_FACTOR=2

RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile --path=/sinatra-sequel/sinatra-sequel/bundle
