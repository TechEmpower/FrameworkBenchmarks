. ../toolset/setup/linux/bash_functions.sh

fw_exists perl-5.18
[ $? -ne 0 ] || { echo "Perl is installed!"; return 0; }

fw_get https://raw.githubusercontent.com/tokuhirom/Perl-Build/master/perl-build -O perl-build.pl
perl perl-build.pl -DDEBUGGING=-g 5.18.2 perl-5.18

fw_get http://cpanmin.us -O cpanminus.pl
perl-5.18/bin/perl cpanminus.pl --notest --no-man-page App::cpanminus
perl-5.18/bin/cpanm -f --notest --no-man-page DBI DBD::mysql Kelp Dancer Mojolicious Kelp::Module::JSON::XS Dancer::Plugin::Database Starman Plack JSON Web::Simple DBD::Pg JSON::XS EV HTTP::Parser::XS Monoceros EV IO::Socket::IP IO::Socket::SSL Memoize
