FROM tfb/vapor-base:latest
CMD .build/release/vapor-tfb-mongodb --env=production
