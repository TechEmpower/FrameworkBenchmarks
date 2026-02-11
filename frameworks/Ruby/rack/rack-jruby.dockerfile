FROM jruby:10.0

RUN apt-get update -y && apt-get install netbase -y

WORKDIR /rack

COPY Gemfile*  ./

#RUN echo $(ruby config/auto_tune.rb | grep -Eo '[0-9]+' | head -n 1)

RUN bundle config set with 'puma'
RUN bundle install --jobs=8

COPY . .

ENV WEB_CONCURRENCY=0

EXPOSE 8080

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production
