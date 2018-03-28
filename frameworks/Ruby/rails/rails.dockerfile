FROM tfb/ruby-2.4:latest

ADD ./ /rails

WORKDIR /rails

RUN bundle install --jobs=4 --gemfile=/rails/Gemfile --path=/rails/rails/bundle

CMD DB_HOST=TFB-database bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
