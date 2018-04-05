FROM techempower/sinatra-sequel-base:0.1

ENV DBTYPE=postgresql
CMD bundle exec unicorn -c config/mri_unicorn.rb -o 0.0.0.0 -p 8080 -E production
