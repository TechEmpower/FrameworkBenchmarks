FROM techempower/ruby-2.4:0.1

ADD ./ /rack-sequel

WORKDIR /rack-sequel

RUN bundle install --jobs=4 --gemfile=/rack-sequel/Gemfile --path=/rack-sequel/rack-sequel/bundle
