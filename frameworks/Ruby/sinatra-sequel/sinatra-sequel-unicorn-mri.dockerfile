FROM tfb/sinatra-sequel-base:latest

ENV DBTYPE=mysql
CMD bundle exec unicorn -c config/mri_unicorn.rb -o 0.0.0.0 -p 8080 -E production
