FROM tfb/roda-sequel-jruby-base:latest

ENV DBTYPE=postgresql
CMD bundle exec torquebox run --io-threads $(( MAX_CONCURRENCY / 2 )) --worker-threads $MAX_CONCURRENCY -b 0.0.0.0 -p 8080 -e production
