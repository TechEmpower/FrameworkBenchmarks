FROM ruby:2.7

ADD ./ /roda-sequel
WORKDIR /roda-sequel

RUN bundle install --jobs=4 --gemfile=/roda-sequel/Gemfile --path=/roda-sequel/roda-sequel/bundle

# TODO: https://github.com/phusion/passenger/issues/1916
ENV _PASSENGER_FORCE_HTTP_SESSION=true
ENV DBTYPE=mysql

RUN ruby -r /roda-sequel/config/auto_tune -e 'puts auto_tune.first' > instances

EXPOSE 8080

CMD bundle exec passenger start --log-level 1 \
       --engine builtin --disable-turbocaching --disable-security-update-check \
       --spawn-method direct --max-pool-size $(cat instances) --min-instances $(cat instances) --max-request-queue-size 1024 \
       --address 0.0.0.0 --port 8080 --environment production
