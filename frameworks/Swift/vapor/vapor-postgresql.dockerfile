FROM techempower/vapor-base:0.1

CMD .build/release/vapor-tfb-postgresql --env=production
