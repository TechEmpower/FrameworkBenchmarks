# Creating your Model

To create a model for the table `members` in your database, create the file `application/classes/model/member.php` with the following syntax:

	class Model_Member extends ORM
	{
		...
	}

(this should provide more examples)

## Overriding the Table name

If you wish to change the database table that a model uses, just override the `$_table_name` variable like this:

	protected $_table_name = 'strange_tablename';

## Changing the primary key

ORM assumes each model (and database table) has an `id` column that is indexed and unique. If your primary key column isn't named `id`, that's fine - just override the `$_primary_key` variable like this:

	protected $_primary_key = 'strange_pkey';

## Use a non-default database

For each model, you can define which database configuration ORM will run queries on. If you override the `$_db_group` variable in your model, ORM will connect to that database. Example:

	protected $_db_group = 'alternate';
