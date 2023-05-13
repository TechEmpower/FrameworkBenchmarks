FROM ruby:3.2

ADD ./ /roda-sequel
WORKDIR /roda-sequel

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV RUBY_YJIT_ENABLE=1

RUN bundle install --jobs=8

ENV DBTYPE=postgresql

EXPOSE 8080

CMD bundle exec unicorn -c config/mri_unicorn.rb -o 0.0.0.0 -p 8080 -E production
