FROM jruby:10.0

RUN apt-get update -y && apt-get install netbase -y

WORKDIR /rack

COPY Gemfile  ./

RUN bundle config set with 'puma'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

CMD config/java_tune.sh

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production
