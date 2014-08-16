# Setup

* Perl 5.10+
* MySQL 5.5
* Wrk 2.0

# Requirements

* Kelp (install from CPAN)
* Kelp::Module::JSON::XS (install from CPAN)
* DBD::mysql (install from CPAN)
* Starman (install from CPAN)
* nginx (if you want to front with nginx, nginx.conf provided)

# Deployment

## uWSGI (Recommended)

1. Create a configuration file. Bare bones example of app.ini:

    [uwsgi]
    http-socket = :8080
    psgi = app.pl

2. Make sure you have installed the psgi plugin.

3. Deploy with uwsgi

    uwsgi --http-modifier1 5 --plugin psgi --ini app.ini

## Plack + Starman

1. Deploy via plackup

    plackup -E production -s Starman --workers=5 -l /tmp/frameworks-benchmark.sock -a ./app.pl

2. If you want to front it with nginx, otherwise

    plackup -E production -s Starman --port=8080 --workers=5 -a ./app.pl
    
# Expert contact
@naturalist (minimal@cpan.org)
