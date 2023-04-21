
FROM ruby:3.2

ADD ./ /rack

WORKDIR /rack

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV RUBY_YJIT_ENABLE=1

RUN bundle install --jobs=8

EXPOSE 8080

CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production

