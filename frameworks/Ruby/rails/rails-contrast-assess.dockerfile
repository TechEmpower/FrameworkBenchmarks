# Ruby Contrast Agent doesn't support 3.0 yet
FROM ruby:2.7

ENV BUNDLE_WITHOUT=mysql
ENV RAILS_ENV=production_postgresql
ENV PORT=8080

EXPOSE 8080
WORKDIR /rails

COPY ./Gemfile* /rails/

RUN bundle install --jobs=8

COPY . /rails/

# Start Contrast Additions
COPY contrast-agent.gem contrast-agent.gem
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=true
ENV CONTRAST__PROTECT__ENABLE=false

run bundle exec gem install ./contrast-agent.gem --platform ruby

RUN bundle add contrast-agent
# End Contrast Additions

CMD ["rails", "server"]