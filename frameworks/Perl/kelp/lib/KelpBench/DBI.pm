package KelpBench::DBI;

use v5.36;
use Kelp::Base 'Kelp';
use DBI;

attr dbh => sub {
	DBI->connect(
		"dbi:MariaDB:database=hello_world;host=tfb-database;port=3306",
		'benchmarkdbuser',
		'benchmarkdbpass',
		{ RaiseError => 1, PrintError => 0 }
	);
};

attr _world => sub ($self) {
	$self->dbh->prepare("SELECT * FROM World WHERE id = ?");
};

attr _fortune => sub ($self) {
	$self->dbh->prepare("SELECT * FROM Fortune");
};

attr _update => sub ($self) {
	$self->dbh->prepare("UPDATE World SET randomNumber = ? WHERE id = ?");
};

sub random_number ($self, $id)
{
	$self->_world->execute($id);
	return $self->_world->fetchrow_hashref;
}

sub fortune ($self)
{
	$self->_fortune->execute();
	return $self->_fortune->fetchall_arrayref({});
}

sub update ($self, $id, $random_number)
{
	$self->_update->execute($random_number, $id);
	return;
}

1;

