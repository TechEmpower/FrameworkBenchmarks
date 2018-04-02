FROM techempower/ruby-2.4:0.1

ADD ./ /sinatra

WORKDIR /sinatra

RUN bundle install --jobs=4 --gemfile=/sinatra/Gemfile --path=/sinatra/sinatra/bundle
