<?php
use ActiveRecord\Column;

include 'helpers/config.php';
require_once __DIR__ . '/../lib/adapters/PgsqlAdapter.php';

class PgsqlAdapterTest extends AdapterTest
{
	public function set_up($connection_name=null)
	{
		parent::set_up('pgsql');
	}

	public function test_insert_id()
	{
		$this->conn->query("INSERT INTO authors(author_id,name) VALUES(nextval('authors_author_id_seq'),'name')");
		$this->assert_true($this->conn->insert_id('authors_author_id_seq') > 0);
	}

	public function test_insert_id_with_params()
	{
		$x = array('name');
		$this->conn->query("INSERT INTO authors(author_id,name) VALUES(nextval('authors_author_id_seq'),?)",$x);
		$this->assert_true($this->conn->insert_id('authors_author_id_seq') > 0);
	}

	public function test_insert_id_should_return_explicitly_inserted_id()
	{
		$this->conn->query('INSERT INTO authors(author_id,name) VALUES(99,\'name\')');
		$this->assert_true($this->conn->insert_id('authors_author_id_seq') > 0);
	}

	public function test_set_charset()
	{
		$connection_string = ActiveRecord\Config::instance()->get_connection($this->connection_name);
		$conn = ActiveRecord\Connection::instance($connection_string . '?charset=utf8');
		$this->assert_equals("SET NAMES 'utf8'",$conn->last_query);
	}

	public function test_gh96_columns_not_duplicated_by_index()
	{
		$this->assert_equals(3,$this->conn->query_column_info("user_newsletters")->rowCount());
	}
}
?>
