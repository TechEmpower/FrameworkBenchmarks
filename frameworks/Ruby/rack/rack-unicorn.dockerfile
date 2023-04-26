FROM ruby:3.2

#RUN apt-get update -yqq && apt-get install -yqq nginx

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV RUBY_YJIT_ENABLE=1

WORKDIR /rack

COPY Gemfile Gemfile.lock ./
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

#CMD nginx -c /rack/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb

CMD bundle exec unicorn -c config/unicorn.rb -o 0.0.0.0 -p 8080 -E production
