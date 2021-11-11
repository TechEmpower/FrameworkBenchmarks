use strict;
use Unix::Processors;
use Getopt::Std 'getopts'; getopts('a', my $opts = +{});
use Cwd 'getcwd';

my $cpus = Unix::Processors->new->max_online;

my @cmd = (
    ($opts->{a} # async server
     ? [qw'plackup -s Twiggy::Prefork -E production --max-reqs-per-child=0 --backlog 16384
           --max-workers', $cpus, qw'-l /dev/shm/app.sock -a app-async.psgi']
     : [qw'start_server --backlog 16384 --path /dev/shm/app.sock --
           plackup -s Gazelle -E production --max-reqs-per-child 10000000
           --max-workers', $cpus, qw'-a app.psgi']),
    [qw'nginx -c nginx.conf -p', getcwd]
);

my @child;
for (@cmd) {
    if ((my $pid = fork) > 0) { push @child, $pid }
    elsif (defined $pid) { exec @$_ }
    else { die 'fork failed' }
}

# wait childs
$SIG{INT} = $SIG{TERM} = sub { kill TERM => @child; 1 while wait != -1 };
1 while wait != -1;

exit 0;
