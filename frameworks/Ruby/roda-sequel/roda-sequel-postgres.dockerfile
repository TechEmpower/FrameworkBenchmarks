FROM techempower/roda-sequel-base:0.1

ENV DBTYPE=postgresql
CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
