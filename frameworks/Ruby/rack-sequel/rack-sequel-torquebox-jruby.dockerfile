FROM techempower/rack-sequel-jruby-base:0.1

ENV DBTYPE=mysql
CMD bundle exec torquebox run --io-threads $(( MAX_CONCURRENCY / 2 )) --worker-threads $MAX_CONCURRENCY -b 0.0.0.0 -p 8080 -e production
