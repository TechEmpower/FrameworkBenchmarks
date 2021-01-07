FROM jruby:9.1

ADD ./ /rack-sequel

WORKDIR /rack-sequel

ENV THREAD_FACTOR=2

RUN bundle install --jobs=4 --gemfile=/rack-sequel/Gemfile --path=/rack-sequel/rack-sequel/bundle

ENV DBTYPE=postgresql

EXPOSE 8080

CMD export MAX_CONCURRENCY=$(( 2 * $(nproc) )) && bundle exec torquebox run --io-threads $(( MAX_CONCURRENCY / 2 )) --worker-threads $MAX_CONCURRENCY -b 0.0.0.0 -p 8080 -e production
