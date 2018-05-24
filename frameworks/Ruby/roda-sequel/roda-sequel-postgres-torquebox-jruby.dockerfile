FROM jruby:9.1

ADD ./ /roda-sequel
WORKDIR /roda-sequel

ENV THREAD_FACTOR=2

RUN bundle install --jobs=4 --gemfile=/roda-sequel/Gemfile --path=/roda-sequel/roda-sequel/bundle

ENV DBTYPE=postgresql
CMD export MAX_CONCURRENCY=$(( 2 * $(nproc) )) && bundle exec torquebox run --io-threads $(( MAX_CONCURRENCY / 2 )) --worker-threads $MAX_CONCURRENCY -b 0.0.0.0 -p 8080 -e production
