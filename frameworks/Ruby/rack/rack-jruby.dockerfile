FROM jruby:9.4-jdk17

RUN apt-get update -y && apt-get install netbase -y

WORKDIR /rack

COPY Gemfile  ./

RUN bundle config set without 'development test'
RUN bundle install --jobs=8

#There is a mismatch in bundler versions so we have to do this
RUN cp Gemfile.lock Gemfile.lock.jruby

COPY . .

RUN cp Gemfile.lock.jruby Gemfile.lock

EXPOSE 8080

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production
