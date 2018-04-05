FROM techempower/ruby-2.4:0.1

ADD ./ /roda-sequel

WORKDIR /roda-sequel

RUN bundle install --jobs=4 --gemfile=/roda-sequel/Gemfile --path=/roda-sequel/roda-sequel/bundle
