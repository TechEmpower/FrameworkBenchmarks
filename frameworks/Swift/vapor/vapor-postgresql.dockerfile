FROM tfb/vapor-base:latest
CMD .build/release/vapor-tfb-postgresql --env=production
