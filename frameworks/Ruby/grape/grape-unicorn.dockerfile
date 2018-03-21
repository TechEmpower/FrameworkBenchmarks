FROM tfb/nginx:latest

FROM tfb/ruby-2.4:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile --path=/grape/grape/bundle

CMD nginx -c /grape/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb
