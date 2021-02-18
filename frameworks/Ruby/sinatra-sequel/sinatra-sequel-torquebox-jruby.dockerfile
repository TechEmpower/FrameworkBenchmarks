FROM jruby:9.1

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

ENV THREAD_FACTOR=2

RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile --path=/sinatra-sequel/sinatra-sequel/bundle

ENV DBTYPE=mysql

EXPOSE 8080

CMD export MAX_CONCURRENCY=$(( 2 * $(nproc) )) && bundle exec torquebox run --io-threads $(( MAX_CONCURRENCY / 2 )) --worker-threads $MAX_CONCURRENCY -b 0.0.0.0 -p 8080 -e production
