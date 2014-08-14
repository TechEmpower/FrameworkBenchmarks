# Setup

* Perl 5.16.3
* MongoDB 2.4.9
* Wrk 2.0

# Requirements

* Mojolicious
* Mango
* JSON::XS
* Hypnotoad provided by Mojolicious

# Deployment

Set production mode:

    export MOJO_MODE=production

Start with Mojolicious' non-blocking preforking server

    hypnotoad app.pl

To stop again, simply run

    hypnotoad -s app.pl

