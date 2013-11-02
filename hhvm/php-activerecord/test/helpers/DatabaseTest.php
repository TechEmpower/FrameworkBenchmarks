<?php
require_once 'DatabaseLoader.php';

class DatabaseTest extends SnakeCase_PHPUnit_Framework_TestCase
{
	protected $conn;
	public static $log = false;

	public function set_up($connection_name=null)
	{
		ActiveRecord\Table::clear_cache();

		$config = ActiveRecord\Config::instance();
		$this->original_default_connection = $config->get_default_connection();

		if ($connection_name)
			$config->set_default_connection($connection_name);

		if ($connection_name == 'sqlite' || $config->get_default_connection() == 'sqlite')
		{
			// need to create the db. the adapter specifically does not create it for us.
			$this->db = substr(ActiveRecord\Config::instance()->get_connection('sqlite'),9);
			new SQLite3($this->db);
		}

		$this->connection_name = $connection_name;
		try {
			$this->conn = ActiveRecord\ConnectionManager::get_connection($connection_name);
		} catch (ActiveRecord\DatabaseException $e) {
			$this->mark_test_skipped($connection_name . ' failed to connect. '.$e->getMessage());
		}

		$GLOBALS['ACTIVERECORD_LOG'] = false;

		$loader = new DatabaseLoader($this->conn);
		$loader->reset_table_data();

		if (self::$log)
			$GLOBALS['ACTIVERECORD_LOG'] = true;
	}

	public function tear_down()
	{
		if ($this->original_default_connection)
			ActiveRecord\Config::instance()->set_default_connection($this->original_default_connection);
	}

	public function assert_exception_message_contains($contains, $closure)
	{
		$message = "";

		try {
			$closure();
		} catch (ActiveRecord\UndefinedPropertyException $e) {
			$message = $e->getMessage();
		}

		$this->assert_true(strpos($message,$contains) !== false);
	}

	/**
	 * Returns true if $regex matches $actual.
	 *
	 * Takes database specific quotes into account by removing them. So, this won't
	 * work if you have actual quotes in your strings.
	 */
	public function assert_sql_has($needle, $haystack)
	{
		$needle = str_replace(array('"','`'),'',$needle);
		$haystack = str_replace(array('"','`'),'',$haystack);
		return $this->assert_true(strpos($haystack,$needle) !== false);
	}

	public function assert_sql_doesnt_has($needle, $haystack)
	{
		$needle = str_replace(array('"','`'),'',$needle);
		$haystack = str_replace(array('"','`'),'',$haystack);
		return $this->assert_false(strpos($haystack,$needle) !== false);
	}
}
?>
