# Setup

* Perl 5.16.3
* MySQL 5.5
* Wrk 2.0

# Requirements

* Mojolicious
* Mojolicious::Plugin::Database
* DBD::mysql
* Starman (if using Starman as web server)
* Plack (for plackup)
* nginx (if you want to front Mojolicious
with nginx, nginx.conf provided)
* Morbo and Hypnotoad provided by Mojolicious

# Deployment

Set production mode:

    export MOJO_MODE=production

Something along the lines of

    plackup -s Starman --workers=2 -l /tmp/frameworks-benchmark.sock -a ./app.pl

if you want to front it with nginx, otherwise

    plackup -s Starman --port 8080 --workers=2 -a ./app.pl

or the equivalent Morbo or Hypnotoad commands.
