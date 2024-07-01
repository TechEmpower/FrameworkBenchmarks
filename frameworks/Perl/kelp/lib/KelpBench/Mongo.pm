package KelpBench::Mongo;

use v5.36;
use Kelp::Base 'Kelp';
use MongoDB;

attr dbh => sub {
	MongoDB::MongoClient->new(
		host => 'tfb-database',
		port => 27017
	)->get_database('hello_world');
};

attr _world => sub ($self) {
	$self->dbh->get_collection('world');
};

attr _fortune => sub ($self) {
	$self->dbh->get_collection('fortune');
};

sub random_number ($self, $id)
{
	return $self->_world->find_one({ _id => $id });
}

sub fortune ($self)
{
	return [$self->_fortune->find->all];
}

sub update ($self, $id, $random_number)
{
	$self->_world->update_one(
		{ _id => $id },
		{ '$set' => { randomNumber => $random_number } },
	);

	return;
}

1;

