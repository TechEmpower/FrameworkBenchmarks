FROM tfb/ruby-2.4:latest

ADD ./ /padrino

WORKDIR /padrino

RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile --path=/padrino/padrino/bundle

CMD DB_HOST=TFB-database bundle exec puma -C config/puma.rb -w 8 --preload
