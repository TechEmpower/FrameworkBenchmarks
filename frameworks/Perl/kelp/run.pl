#!/usr/bin/env perl

use v5.36;
use Data::Dumper;

my $max_reqs = $ENV{MAX_REQS};
my $test_name = $ENV{TEST_NAME};
my $socket_file = $ENV{SOCKET_FILE};
my $app_runner = 'app.psgi';

my $max_workers = `nproc`;
chomp $max_workers;

my %runner_map = (
	gazelle => [
		'start_server',
		'--path' => $socket_file,
		'--backlog' => 16384,
		'--',
		'plackup',
		'-E' => 'production',
		'-s' => 'Gazelle',
		'--max-workers' => $max_workers,
		'--max-reqs-per-child' => $max_reqs,
		'-a' => $app_runner,
	],
	starman => [
		'start_server',
		'--backlog' => 16384,
		'--',
		'plackup',
		'-E' => 'production',
		'-s' => 'Starman',
		'-l' => $socket_file,
		'--workers' => $max_workers,
		'--max-requests' => $max_reqs,
		'-a' => $app_runner,
	],
	starlet => [
		'start_server',
		'--path' => $socket_file,
		'--backlog' => 16384,
		'--',
		'plackup',
		'-E' => 'production',
		'-s' => 'Starlet',
		'--max-workers' => $max_workers,
		'--max-reqs-per-child' => $max_reqs,
		'-a' => $app_runner,
	],
	# NOTE: twiggy does not play well with Server::Starter
	# NOTE: twiggy couldn't pass update tests, so I disabled them
	twiggy => [
		'plackup',
		'-E' => 'production',
		'-s' => 'Twiggy::Prefork',
		'-l' => $socket_file,
		'--backlog' => 16384,
		'--max-workers' => $max_workers,
		'--max-reqs-per-child' => $max_reqs,
		'-a' => $app_runner,
	],
);

die "invalid test name $test_name"
	unless $test_name =~ m{^kelp-(\w+)-(\w+)$};

die 'database mismatch'
	unless $2 eq $ENV{DATABASE};

my $command = $runner_map{$1};
die "invalid server $1"
	unless $command;

say 'Running command: ' . Dumper($command);

exec @$command;

