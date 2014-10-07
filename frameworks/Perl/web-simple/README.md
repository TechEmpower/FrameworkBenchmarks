# Setup

* Perl 5.10+
* MySQL 5.5
* Wrk 2.0

# Requirements

* Web::Simple
* DBD::mysql
* Starman (if using Starman as web server)
* Plack (for plackup)
* nginx (if you want to front Dancer with nginx, nginx.conf provided)

# Deployment

Something along the lines of

    plackup -E production -s Starman --workers=8 -l /tmp/frameworks-benchmark.sock -a ./app.pl

if you want to front it with nginx, otherwise

    plackup -E production -s Starman --port=8080 --workers=8 -a ./app.pl
