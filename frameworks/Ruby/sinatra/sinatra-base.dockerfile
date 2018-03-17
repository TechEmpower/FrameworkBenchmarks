FROM tfb/ruby-2.4:latest

ADD ./ /sinatra

WORKDIR /sinatra

RUN bundle install --jobs=4 --gemfile=/sinatra/Gemfile --path=/sinatra/sinatra/bundle
