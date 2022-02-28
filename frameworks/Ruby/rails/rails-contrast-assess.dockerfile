FROM ruby:3.1

RUN apt-get update -yqq && apt-get install -yqq --no-install-recommends redis-server

EXPOSE 8080
WORKDIR /rails

COPY ./Gemfile* /rails/

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV BUNDLE_WITHOUT=mysql
RUN bundle install --jobs=8

COPY . /rails/

ENV RUBY_YJIT_ENABLE=1
ENV RAILS_ENV=production_postgresql
ENV PORT=8080
ENV REDIS_URL=redis://localhost:6379/0/cache

# Start Contrast Additions
COPY contrast-agent.gem contrast-agent.gem
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=true
ENV CONTRAST__PROTECT__ENABLE=false

run bundle exec gem install ./contrast-agent.gem --platform ruby

RUN bundle add contrast-agent
# End Contrast Additions

CMD ./run-with-redis.sh
