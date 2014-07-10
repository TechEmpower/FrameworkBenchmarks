# About

Plack

# Setup

* Perl 5.12+
* MySQL 5.5
* Wrk 2.0

# Requirements

* Plack
* Starlet
* HTTP::Parser::XS
* JSON::XS
* DBI
* DBD::mysql

# Deployment

    plackup -E production -s Starlet --max-keepalive-reqs 5000 \
      --max-reqs-per-child 50000 --min-reqs-per-child 40000 --workers=2 -l :8080 app.psgi
