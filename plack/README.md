# About

Plack

# Setup

* Perl 5.12+
* MySQL 5.5
* Wrk 2.0

# Requirements

* Plack
* Monoceros or Starman
* EV
* HTTP::Parser::XS
* JSON::XS
* DBI
* DBD::mysql

# Deployment

    plackup -E production -s Starman --workers=2 -l :8080 app.psgi
    - or -
    plackup -E production -s Monoceros --max-workers=2 -l :8080 app.psgi
