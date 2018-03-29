FROM techempower/nginx:0.1

FROM techempower/ruby-2.4:0.1

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /rails

WORKDIR /rails

RUN bundle install --jobs=4 --gemfile=/rails/Gemfile --path=/rails/rails/bundle

CMD nginx -c /rails/config/nginx.conf && \
    DB_HOST=tfb-database bundle exec unicorn_rails -E production -c config/unicorn.rb
