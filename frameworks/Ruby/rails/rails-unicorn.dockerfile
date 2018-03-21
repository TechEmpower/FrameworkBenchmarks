FROM tfb/nginx:latest

FROM tfb/ruby-2.4:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /rails

WORKDIR /rails

RUN bundle install --jobs=4 --gemfile=/rails/Gemfile --path=/rails/rails/bundle

CMD nginx -c /rails/config/nginx.conf && \
    DB_HOST=TFB-database bundle exec unicorn_rails -E production -c config/unicorn.rb
