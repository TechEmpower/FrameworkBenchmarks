FROM tfb/nginx:latest

FROM tfb/ruby-2.4:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /rack

WORKDIR /rack

RUN bundle install --jobs=4 --gemfile=/rack/Gemfile --path=/rack/rack/bundle

CMD nginx -c /rack/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb
