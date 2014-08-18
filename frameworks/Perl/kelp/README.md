# Setup

* Perl 5.10+
* MySQL 5.5
* MongoDB
* Wrk 2.0

# Requirements

* Kelp (install from CPAN)
* Kelp::Module::JSON::XS (install from CPAN)
* Kelp::Module::Template::Toolkit (install from CPAN)
* DBD::mysql (install from CPAN)
* Starman (install from CPAN)
* MongoDB (install from CPAN)
* nginx (if you want to front with nginx, nginx.conf provided)

# Deployment

## uWSGI (recommended)

1. Make sure you have installed the psgi plugin.
2. Deploy:

    ./uwsgi --plugins psgi --init app.ini

## Plack + Starman

1. Deploy via plackup

    plackup -E deployment -s Starman --workers=25 -l /tmp/frameworks-benchmark.sock -a ./app.pl

2. If you want to front it with nginx, otherwise

    plackup -E deployment -s Starman --port=8080 --workers=25 -a ./app.pl

# Expert contact

@naturalist (minimal@cpan.org)
