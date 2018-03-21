FROM tfb/ruby-2.4:latest

ADD ./ /sinatra-sequel

WORKDIR /sinatra-sequel

RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile --path=/sinatra-sequel/sinatra-sequel/bundle
