FROM techempower/nginx:0.1

FROM techempower/ruby-2.4:0.1

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /padrino

WORKDIR /padrino

RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile --path=/padrino/padrino/bundle

CMD nginx -c /padrino/config/nginx.conf && \
    DB_HOST=tfb-database bundle exec unicorn -E production -c config/unicorn.rb
