<?php
use ActiveRecord\Column;

include 'helpers/config.php';
require_once __DIR__ . '/../lib/adapters/MysqlAdapter.php';

class MysqlAdapterTest extends AdapterTest
{
	public function set_up($connection_name=null)
	{
		parent::set_up('mysql');
	}

	public function test_enum()
	{
		$author_columns = $this->conn->columns('authors');
		$this->assert_equals('enum',$author_columns['some_enum']->raw_type);
		$this->assert_equals(Column::STRING,$author_columns['some_enum']->type);
		$this->assert_same(null,$author_columns['some_enum']->length);
	}

	public function test_set_charset()
	{
		$connection_string = ActiveRecord\Config::instance()->get_connection($this->connection_name);
		$conn = ActiveRecord\Connection::instance($connection_string . '?charset=utf8');
		$this->assert_equals('SET NAMES ?',$conn->last_query);
	}

	public function test_limit_with_null_offset_does_not_contain_offset()
	{
		$ret = array();
		$sql = 'SELECT * FROM authors ORDER BY name ASC';
		$this->conn->query_and_fetch($this->conn->limit($sql,null,1),function($row) use (&$ret) { $ret[] = $row; });

		$this->assert_true(strpos($this->conn->last_query, 'LIMIT 1') !== false);
	}
}
?>
