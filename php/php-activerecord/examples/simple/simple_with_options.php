<?php
require_once __DIR__ . '/../../ActiveRecord.php';

class Book extends ActiveRecord\Model
{
	// explicit table name since our table is not "books"
	static $table_name = 'simple_book';

	// explicit pk since our pk is not "id"
	static $primary_key = 'book_id';

	// explicit connection name since we always want production with this model
	static $connection = 'production';

	// explicit database name will generate sql like so => db.table_name
	static $db = 'test';
}

$connections = array(
	'development' => 'mysql://invalid',
	'production' => 'mysql://test:test@127.0.0.1/test'
);

// initialize ActiveRecord
ActiveRecord\Config::initialize(function($cfg) use ($connections)
{
    $cfg->set_model_directory('.');
    $cfg->set_connections($connections);
});

print_r(Book::first()->attributes());
?>
