FROM tfb/vapor-base:latest
CMD .build/release/vapor-tfb-mysql --env=production
