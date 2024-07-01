# Setup

* Perl 5.36+
* MariaDB or MongoDB

# Requirements

* Kelp (install from CPAN)
* Kelp::Module::Template::Toolkit (install from CPAN)
* DBI + DBD::mysql or MongoDB (install from CPAN)
* Gazelle (install from CPAN)
* Starman (install from CPAN)
* Starlet (install from CPAN)
* Twiggy::Prefork (install from CPAN)
* nginx (if you want to front with nginx, nginx.conf provided)

# Deployment

## uWSGI (recommended)

1. Make sure you have installed the psgi plugin.
2. Deploy:

    ./uwsgi --plugins psgi --init app.ini

## Plack + plack handler

Recommended handler is `Gazelle`.

1. Deploy via `start_server`, if you want to front it with nginx.

    start_server --path /tmp/perl-kelp.sock --backlog 16384 -- plackup -E production -s Gazelle --max-workers=25 --max-reqs-per-child=10000 -a ./app.psgi

2. Otherwise

    plackup -E deployment -s Gazelle --port=8080 --max-workers=25 -a ./app.psgi

# Code information

`lib/KelpBench.pm` contains all action-handling and helper code. It is a full
Kelp app with `Template::Toolkit` module and standard Kelp configuration files.
While it could've been coded as a one-file Kelp app, full app style gives us
more control on the behavior of the app.  It lazy-loads `DBI.pm` or `Mongo.pm`
from `lib/KelpBench/` directory based on environmental variable `MONGO`, so it
only needs one database driver at a time.

The app is written in a relaxed style, not trying very hard to achieve the best
possible result. It very much resembles production code. For example, a proper
templating engine is used to produce the HTML document instead of inline HTML
(which is obviously much faster).

App can be tested using mock database by running `prove -l`. In this case, it
only requires `Kelp` and `Kelp::Module::Template::Toolkit` from CPAN to be
installed.

# Expert contact

@bbrtj (contact@bbrtj.eu)
@naturalist (minimal@cpan.org)

