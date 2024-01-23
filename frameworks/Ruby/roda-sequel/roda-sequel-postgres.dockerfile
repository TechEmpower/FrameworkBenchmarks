FROM ruby:3.3

ADD ./ /roda-sequel
WORKDIR /roda-sequel

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV RUBY_YJIT_ENABLE=1

RUN bundle install --jobs=8

ENV DBTYPE=postgresql

EXPOSE 8080

CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
