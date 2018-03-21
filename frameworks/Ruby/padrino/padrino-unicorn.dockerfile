FROM tfb/nginx:latest

FROM tfb/ruby-2.4:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /padrino

WORKDIR /padrino

RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile --path=/padrino/padrino/bundle

CMD nginx -c /padrino/config/nginx.conf && \
    DB_HOST=TFB-database bundle exec unicorn -E production -c config/unicorn.rb
