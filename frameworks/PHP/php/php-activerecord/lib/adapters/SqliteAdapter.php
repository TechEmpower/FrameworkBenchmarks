<?php
/**
 * @package ActiveRecord
 */
namespace ActiveRecord;

use PDO;

/**
 * Adapter for SQLite.
 *
 * @package ActiveRecord
 */
class SqliteAdapter extends Connection
{
	protected function __construct($info)
	{
		if (!file_exists($info->host))
			throw new DatabaseException("Could not find sqlite db: $info->host");

		$this->connection = new PDO("sqlite:$info->host",null,null,static::$PDO_OPTIONS);
	}

	public function limit($sql, $offset, $limit)
	{
		$offset = is_null($offset) ? '' : intval($offset) . ',';
		$limit = intval($limit);
		return "$sql LIMIT {$offset}$limit";
	}

	public function query_column_info($table)
	{
		return $this->query("pragma table_info($table)");
	}

	public function query_for_tables()
	{
		return $this->query("SELECT name FROM sqlite_master");
	}

	public function create_column($column)
	{
		$c = new Column();
		$c->inflected_name  = Inflector::instance()->variablize($column['name']);
		$c->name            = $column['name'];
		$c->nullable        = $column['notnull'] ? false : true;
		$c->pk              = $column['pk'] ? true : false;
		$c->auto_increment  = in_array(
				strtoupper($column['type']),
				array('INT', 'INTEGER')
			) && $c->pk;

		$column['type'] = preg_replace('/ +/',' ',$column['type']);
		$column['type'] = str_replace(array('(',')'),' ',$column['type']);
		$column['type'] = Utils::squeeze(' ',$column['type']);
		$matches = explode(' ',$column['type']);

		if (!empty($matches))
		{
			$c->raw_type = strtolower($matches[0]);

			if (count($matches) > 1)
				$c->length = intval($matches[1]);
		}

		$c->map_raw_type();

		if ($c->type == Column::DATETIME)
			$c->length = 19;
		elseif ($c->type == Column::DATE)
			$c->length = 10;

		// From SQLite3 docs: The value is a signed integer, stored in 1, 2, 3, 4, 6,
		// or 8 bytes depending on the magnitude of the value.
		// so is it ok to assume it's possible an int can always go up to 8 bytes?
		if ($c->type == Column::INTEGER && !$c->length)
			$c->length = 8;

		$c->default = $c->cast($column['dflt_value'],$this);

		return $c;
	}

	public function set_encoding($charset)
	{
		throw new ActiveRecordException("SqliteAdapter::set_charset not supported.");
	}

	public function accepts_limit_and_order_for_update_and_delete() { return true; }

	public function native_database_types()
	{
		return array(
			'primary_key' => 'integer not null primary key',
			'string' => array('name' => 'varchar', 'length' => 255),
			'text' => array('name' => 'text'),
			'integer' => array('name' => 'integer'),
			'float' => array('name' => 'float'),
			'decimal' => array('name' => 'decimal'),
			'datetime' => array('name' => 'datetime'),
			'timestamp' => array('name' => 'datetime'),
			'time' => array('name' => 'time'),
			'date' => array('name' => 'date'),
			'binary' => array('name' => 'blob'),
			'boolean' => array('name' => 'boolean')
		);
	}

}
?>