FROM tfb/nginx:latest

FROM tfb/ruby-2.4:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /hanami

WORKDIR /hanami

RUN bundle install --jobs=4 --gemfile=/hanami/Gemfile --path=/hanami/hanami/bundle

CMD nginx -c /hanami/config/nginx.conf && \
    RACK_ENV=none DB_HOST=TFB-database bundle exec unicorn_rails -E production -c config/unicorn.rb
