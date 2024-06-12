package KelpBench;

use v5.36;
use Kelp::Base 'Kelp';

## Attributes

attr database => sub {
	if ($ENV{MONGO}) {
		require KelpBench::Mongo;
		return KelpBench::Mongo->new;
	}
	else {
		require KelpBench::DBI;
		return KelpBench::DBI->new;
	}
};

## Utilities

sub validate_number ($self, $num, $min, $max)
{
	return $min unless length($num // '') && $num !~ /\D/;
	return $min if $num < $min;
	return $max if $num > $max;
	return $num;
}

sub random_number ($self, $max = 10_000)
{
	return int(rand($max) + 1);
}

sub random_id ($self)
{
	# in case random ids were not the same as random numbers
	return $self->random_number(10_000);
}

sub get_random_entries ($self, $count)
{
	$count = $self->validate_number($count, 1, 500);

	my @result;
	for (1 .. $count) {
		my $id = $self->random_id;
		my $row = $self->database->random_number($id);
		next unless $row;

		push @result, {
			id => $id,
			randomNumber => $row->{randomNumber}
		};
	}

	return \@result;
}

## Framework code

sub before_dispatch {} # skip trying to log access
sub before_finalize {} # skip adding X-Framework

sub build ($self)
{
	$self->add_route([GET => '/plaintext'] => 'action_plaintext');
	$self->add_route([GET => '/json'] => 'action_json');
	$self->add_route([GET => '/db'] => 'action_db');
	$self->add_route([GET => '/queries'] => 'action_queries');
	$self->add_route([GET => '/fortunes'] => 'action_fortunes');
	$self->add_route([GET => '/updates'] => 'action_updates');
}

## Registered route handlers
## Names prefixed with _action, because we did not separate a controller
## (Controllers would slow this down a bit due to reblessing of app object)

sub action_plaintext ($self)
{
	$self->res->text;
	return 'Hello, World!';
}

sub action_json ($self)
{
	return { message => 'Hello, World!' };
}

sub action_db ($self)
{
	my $id = $self->random_id;
	my $row = $self->database->random_number($id);

	return { id => $id, randomNumber => $row->{randomNumber} };
}

sub action_queries ($self)
{
	return $self->get_random_entries($self->param('queries'));
}

sub action_fortunes ($self) {
	my $objects = $self->database->fortune;

	push $objects->@*, {
		id => 0,
		message => "Additional fortune added at request time."
	};

	$objects->@* = sort { $a->{message} cmp $b->{message} } $objects->@*;
	return $self->template('fortunes', { rows => $objects });
}

sub action_updates ($self)
{
	my $arr = $self->get_random_entries($self->param('queries'));

	foreach my $row ($arr->@*) {
		$row->{randomNumber} = $self->random_number;
		$self->database->update($row->@{qw(id randomNumber)});
	}

	return $arr;
};

1;

