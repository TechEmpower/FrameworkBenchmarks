<?php
/**
 * @package ActiveRecord
 */
namespace ActiveRecord;

/**
 * Helper class for building sql statements progmatically.
 *
 * @package ActiveRecord
 */
class SQLBuilder
{
	private $connection;
	private $operation = 'SELECT';
	private $table;
	private $select = '*';
	private $joins;
	private $order;
	private $limit;
	private $offset;
	private $group;
	private $having;
	private $update;

	// for where
	private $where;
	private $where_values = array();

	// for insert/update
	private $data;
	private $sequence;

	/**
	 * Constructor.
	 *
	 * @param Connection $connection A database connection object
	 * @param string $table Name of a table
	 * @return SQLBuilder
	 * @throws ActiveRecordException if connection was invalid
	 */
	public function __construct($connection, $table)
	{
		if (!$connection)
			throw new ActiveRecordException('A valid database connection is required.');

		$this->connection	= $connection;
		$this->table		= $table;
	}

	/**
	 * Returns the SQL string.
	 *
	 * @return string
	 */
	public function __toString()
	{
		return $this->to_s();
	}

	/**
	 * Returns the SQL string.
	 *
	 * @see __toString
	 * @return string
	 */
	public function to_s()
	{
		$func = 'build_' . strtolower($this->operation);
		return $this->$func();
	}

	/**
	 * Returns the bind values.
	 *
	 * @return array
	 */
	public function bind_values()
	{
		$ret = array();

		if ($this->data)
			$ret = array_values($this->data);

		if ($this->get_where_values())
			$ret = array_merge($ret,$this->get_where_values());

		return array_flatten($ret);
	}

	public function get_where_values()
	{
		return $this->where_values;
	}

	public function where(/* (conditions, values) || (hash) */)
	{
		$this->apply_where_conditions(func_get_args());
		return $this;
	}

	public function order($order)
	{
		$this->order = $order;
		return $this;
	}

	public function group($group)
	{
		$this->group = $group;
		return $this;
	}

	public function having($having)
	{
		$this->having = $having;
		return $this;
	}

	public function limit($limit)
	{
		$this->limit = intval($limit);
		return $this;
	}

	public function offset($offset)
	{
		$this->offset = intval($offset);
		return $this;
	}

	public function select($select)
	{
		$this->operation = 'SELECT';
		$this->select = $select;
		return $this;
	}

	public function joins($joins)
	{
		$this->joins = $joins;
		return $this;
	}

	public function insert($hash, $pk=null, $sequence_name=null)
	{
		if (!is_hash($hash))
			throw new ActiveRecordException('Inserting requires a hash.');

		$this->operation = 'INSERT';
		$this->data = $hash;

		if ($pk && $sequence_name)
			$this->sequence = array($pk,$sequence_name);

		return $this;
	}

	public function update($mixed)
	{
		$this->operation = 'UPDATE';

		if (is_hash($mixed))
			$this->data = $mixed;
		elseif (is_string($mixed))
			$this->update = $mixed;
		else
			throw new ActiveRecordException('Updating requires a hash or string.');

		return $this;
	}

	public function delete()
	{
		$this->operation = 'DELETE';
		$this->apply_where_conditions(func_get_args());
		return $this;
	}

	/**
	 * Reverses an order clause.
	 */
	public static function reverse_order($order)
	{
		if (!trim($order))
			return $order;

		$parts = explode(',',$order);

		for ($i=0,$n=count($parts); $i<$n; ++$i)
		{
			$v = strtolower($parts[$i]);

			if (strpos($v,' asc') !== false)
				$parts[$i] = preg_replace('/asc/i','DESC',$parts[$i]);
			elseif (strpos($v,' desc') !== false)
				$parts[$i] = preg_replace('/desc/i','ASC',$parts[$i]);
			else
				$parts[$i] .= ' DESC';
		}
		return join(',',$parts);
	}

	/**
	 * Converts a string like "id_and_name_or_z" into a conditions value like array("id=? AND name=? OR z=?", values, ...).
	 *
	 * @param Connection $connection
	 * @param $name Underscored string
	 * @param $values Array of values for the field names. This is used
	 *   to determine what kind of bind marker to use: =?, IN(?), IS NULL
	 * @param $map A hash of "mapped_column_name" => "real_column_name"
	 * @return A conditions array in the form array(sql_string, value1, value2,...)
	 */
	public static function create_conditions_from_underscored_string(Connection $connection, $name, &$values=array(), &$map=null)
	{
		if (!$name)
			return null;

		$parts = preg_split('/(_and_|_or_)/i',$name,-1,PREG_SPLIT_DELIM_CAPTURE);
		$num_values = count($values);
		$conditions = array('');

		for ($i=0,$j=0,$n=count($parts); $i<$n; $i+=2,++$j)
		{
			if ($i >= 2)
				$conditions[0] .= preg_replace(array('/_and_/i','/_or_/i'),array(' AND ',' OR '),$parts[$i-1]);

			if ($j < $num_values)
			{
				if (!is_null($values[$j]))
				{
					$bind = is_array($values[$j]) ? ' IN(?)' : '=?';
					$conditions[] = $values[$j];
				}
				else
					$bind = ' IS NULL';
			}
			else
				$bind = ' IS NULL';

			// map to correct name if $map was supplied
			$name = $map && isset($map[$parts[$i]]) ? $map[$parts[$i]] : $parts[$i];

			$conditions[0] .= $connection->quote_name($name) . $bind;
		}
		return $conditions;
	}

	/**
	 * Like create_conditions_from_underscored_string but returns a hash of name => value array instead.
	 *
	 * @param string $name A string containing attribute names connected with _and_ or _or_
	 * @param $args Array of values for each attribute in $name
	 * @param $map A hash of "mapped_column_name" => "real_column_name"
	 * @return array A hash of array(name => value, ...)
	 */
	public static function create_hash_from_underscored_string($name, &$values=array(), &$map=null)
	{
		$parts = preg_split('/(_and_|_or_)/i',$name);
		$hash = array();

		for ($i=0,$n=count($parts); $i<$n; ++$i)
		{
			// map to correct name if $map was supplied
			$name = $map && isset($map[$parts[$i]]) ? $map[$parts[$i]] : $parts[$i];
			$hash[$name] = $values[$i];
		}
		return $hash;
	}

	/**
	 * prepends table name to hash of field names to get around ambiguous fields when SQL builder
	 * has joins
	 *
	 * @param array $hash
	 * @return array $new
	 */
	private function prepend_table_name_to_fields($hash=array())
	{
		$new = array();
		$table = $this->connection->quote_name($this->table);

		foreach ($hash as $key => $value)
		{
			$k = $this->connection->quote_name($key);
			$new[$table.'.'.$k] = $value;
		}

		return $new;
	}

	private function apply_where_conditions($args)
	{
		require_once 'Expressions.php';
		$num_args = count($args);

		if ($num_args == 1 && is_hash($args[0]))
		{
			$hash = is_null($this->joins) ? $args[0] : $this->prepend_table_name_to_fields($args[0]);
			$e = new Expressions($this->connection,$hash);
			$this->where = $e->to_s();
			$this->where_values = array_flatten($e->values());
		}
		elseif ($num_args > 0)
		{
			// if the values has a nested array then we'll need to use Expressions to expand the bind marker for us
			$values = array_slice($args,1);

			foreach ($values as $name => &$value)
			{
				if (is_array($value))
				{
					$e = new Expressions($this->connection,$args[0]);
					$e->bind_values($values);
					$this->where = $e->to_s();
					$this->where_values = array_flatten($e->values());
					return;
				}
			}

			// no nested array so nothing special to do
			$this->where = $args[0];
			$this->where_values = &$values;
		}
	}

	private function build_delete()
	{
		$sql = "DELETE FROM $this->table";

		if ($this->where)
			$sql .= " WHERE $this->where";

		if ($this->connection->accepts_limit_and_order_for_update_and_delete())
		{
			if ($this->order)
				$sql .= " ORDER BY $this->order";

			if ($this->limit)
				$sql = $this->connection->limit($sql,null,$this->limit);
		}

		return $sql;
	}

	private function build_insert()
	{
		require_once 'Expressions.php';
		$keys = join(',',$this->quoted_key_names());

		if ($this->sequence)
		{
			$sql =
				"INSERT INTO $this->table($keys," . $this->connection->quote_name($this->sequence[0]) .
				") VALUES(?," . $this->connection->next_sequence_value($this->sequence[1]) . ")";
		}
		else
			$sql = "INSERT INTO $this->table($keys) VALUES(?)";

		$e = new Expressions($this->connection,$sql,array_values($this->data));
		return $e->to_s();
	}

	private function build_select()
	{
		$sql = "SELECT $this->select FROM $this->table";

		if ($this->joins)
			$sql .= ' ' . $this->joins;

		if ($this->where)
			$sql .= " WHERE $this->where";

		if ($this->group)
			$sql .= " GROUP BY $this->group";

		if ($this->having)
			$sql .= " HAVING $this->having";

		if ($this->order)
			$sql .= " ORDER BY $this->order";

		if ($this->limit || $this->offset)
			$sql = $this->connection->limit($sql,$this->offset,$this->limit);

		return $sql;
	}

	private function build_update()
	{
		if (strlen($this->update) > 0)
			$set = $this->update;
		else
			$set = join('=?, ', $this->quoted_key_names()) . '=?';

		$sql = "UPDATE $this->table SET $set";

		if ($this->where)
			$sql .= " WHERE $this->where";

		if ($this->connection->accepts_limit_and_order_for_update_and_delete())
		{
			if ($this->order)
				$sql .= " ORDER BY $this->order";

			if ($this->limit)
				$sql = $this->connection->limit($sql,null,$this->limit);
		}

		return $sql;
	}

	private function quoted_key_names()
	{
		$keys = array();

		foreach ($this->data as $key => $value)
			$keys[] = $this->connection->quote_name($key);

		return $keys;
	}
}
?>
