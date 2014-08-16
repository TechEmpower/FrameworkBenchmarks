<?php

namespace PHPixie\DB;

/**
 * Query builder. It allows building queries by using methods to set specific query parameters.
 * Database drivers extend this class so that they can generate database specific queries.
 * The idea is to provide a database agnostic interface to query writing.
 *
 * @method mixed table(string $table = null) Set table to query.
 *               Without arguments returns current table, returns self otherwise.
 *
 * @method mixed data(array $data = null) Set data for insert or update queries.
 *               Without arguments returns current data, returns self otherwise.
 *
 * @method mixed limit(int $limit = null) Set number of rows to return. If NULL is passed than no limit is used.
 *               If NULL is passed than no limit is used.
 *               Without arguments returns current limit, returns self otherwise.
 *
 * @method mixed offset(string $offset = null) Set the offset for the first row in result.
 *               If NULL is passed than no offset is used.
 *               Without arguments returns current offset, returns self otherwise.
 *
 * @method mixed group_by(string $group_by = null) A column to group rows by for aggregator functions.
 *               If NULL is passed than no grouping is done.
 *               Without arguments returns current group_by argument, returns self otherwise.
 *
 * @method mixed type(string $type = null) Set query type. Available types: select, update, insert, delete, count.
 *               Without arguments returns current type argument, returns self otherwise.
 * @package Database
 */

abstract class Query
{

	/**
	 * Array of conditions that rows must meet
	 * @var array
	 */
	protected $_conditions = array();

	/**
	 * Table to query
	 * @var unknown
	 */
	protected $_table;

	/**
	 * Fields to return in the query
	 * @var array
	 */
	protected $_fields;

	/**
	 * Data for row insertion or update
	 * @var unknown
	 */
	protected $_data;

	/**
	 * Query type. Available types: select, update, insert, delete, count
	 * @var string
	 */
	protected $_type;

	/**
	 * Parameters for tables to join
	 * @var array
	 */
	protected $_joins = array();

	/**
	 * Number of rows to return
	 * @var int
	 */
	protected $_limit;

	/**
	 * Offset of the first row
	 * @var int
	 */
	protected $_offset;

	/**
	 * Columns and directions to order by
	 * @var array
	 */
	protected $_order_by = array();

	/**
	 * Database connection
	 * @var DB
	 */
	protected $_db;

	/**
	 * Conditions for aggregator functions
	 * @var array
	 */
	protected $_having = array();

	/**
	 * Column to group by for aggregator functions
	 * @var string
	 */
	protected $_group_by;

	/**
	 * Last alias used on the table
	 * @var string
	 */
	protected $_alias = null;

	/**
	 * Methods and type of value they allow that are available via __call
	 * @var array
	 */
	protected $methods = array('data' => 'array', 'limit' => array('integer', 'NULL'), 'offset' => array('integer', 'NULL'), 'group_by' => array('string', 'NULL'), 'type' => 'string');

	/**
	 * UNION queries
	 * @var array
	 */
	protected $_union = array();

	/**
	 * Generates a query in format that can be executed on current database implementation
	 *
	 */
	public abstract function query();

	/**
	 * Creates a new query
	 *
	 * @param DB $db   Database connection
	 * @param string $type Query type. Available types: select, update, insert, delete, count
	 * @return void
	 */
	public function __construct($db, $type)
	{
		$this->_db = $db;
		$this->_type = $type;
	}

	/**
	 * Sets fields to be queried from the database. You can add aliases to the fields
	 * by passing them as:
	 *
	 * array('field_name','alias')
	 *
	 * Example: $query->fields('id', array('name', 'fairy_name'))
	 *
	 * @param mixed   $field,...   Fields to be selected from the table
	 * @return mixed  If no parameters are passed returns current array of fields,
	 *                otherwise returns self.
	 */
	public function fields()
	{
		$p = func_get_args();
		if (empty($p))
		{
			return $this->_fields;
		}
		else
		{
			$this->_fields = $p;
		}
		return $this;
	}

	/**
	 * Sets the table to perform operations on, also supports subqueries
	 *
	 * @param string|Query_database|Expression_database $table table to select from
	 * @param string $alias Alias for this table
	 * @return mixed Returns self if a table is passed, otherwise returns the table
	 */
	public function table($table = null, $alias = null)
	{
		if ($table == null)
		{
			return is_array($this->_table) ? $this->_table[1] : $this->_table;
		}

		if (!is_string($table) && $alias == null)
		{

			$alias = $this->add_alias();
		}
		$this->_table = $alias == null ? $table : array($table, $alias);

		return $this;
	}

	/**
	 * Magic methods to create methods for all generic query parts
	 *
	 * @param string    $method Name of the method to call
	 * @param array     $args   Array of parameters
	 * @return mixed    If no arguments are passed returns the current value of the property,
	 *                  otherwise returns self.
	 * @throws \Exception If method doesn't exist
	 * @throws \Exception If value is of incorrect type
	 * @see $methods
	 */

	public function __call($method, $args)
	{
		if (isset($this->methods[$method]))
		{
			$property = '_'.$method;

			if (empty($args))
			{
				return $this->$property;
			}
			$val = $args[0];
			if (is_numeric($val))
			{
				$val = (int) $val;
			}
			$allowed_types = $this->methods[$method];
			if (!is_array($allowed_types))
			{
				$allowed_types = array($allowed_types);
			}
			if (!in_array(gettype($val), $allowed_types))
			{
				throw new \Exception("Method '{$method}' only accepts values of type: ".implode(' or ', $allowed_types).", '{$val}' was passed");
			}
			$this->$property = $val;
			return $this;
		}
		throw new \Exception("Method '{$method}' doesn't exist.");
	}

	/**
	 * Executes the query
	 *
	 * @return object Executes current query on its database connection
	 * @see DB
	 */
	public function execute()
	{
		$query = $this->query();
		$result = $this->_db->execute($query[0], $query[1]);
		if ($this->_type == 'count')
		{
			return $result->get('count');
		}
		return $result;
	}

	/**
	 * Adds a joined table to the query.
	 *
	 * @param string $table Table to join
	 * @param array $conds Conditions to join tables on, same behavior as with where() method
	 * @param string  $type  Type of join. Defaults to 'left'
	 * @return Query_Database   Returns self
	 * @see where()
	 */
	public function join($table, $conds, $type = 'left')
	{
		$this->_joins[] = array($table, $type, $this->get_condition_part($conds));
		return $this;
	}

	/**
	 * Sets conditions for aggregate functions, same behavior as with where() method
	 *
	 * @return Query_Database Returns self
	 * @see where()
	 */
	public function having()
	{
		$p = func_get_args();
		$cond = $this->get_condition_part($p);
		$this->_having = array_merge($this->_having, array($cond));
		return $this;
	}

	/**
	 * Adds a column to ordering parameters.
	 *
	 * @param string $column Column to order by
	 * @param string $dir Ordering direction.
	 * @return Query_Database  Returns self
	 * @throws \Exception If ordering direction isn't DESC or ASC
	 */
	public function order_by($column, $dir = 'ASC')
	{
		$dir = strtoupper($dir);
		if ($dir != 'DESC' && $dir != 'ASC')
		{
			throw new \Exception("Invalid sorting direction {$dir} specified");
		}
		$this->_order_by[] = array($column, $dir);
		return $this;
	}

	/**
	 * Sets conditions for the query.
	 * Can be called in many ways, examples:
	 * Shorthand equals condition:
	 * <code>
	 * $q->where('name', 'Tinkerbell')
	 * </code>
	 * Conditions with operator:
	 * <code>
	 * $q->where('id', '>', 3)
	 * </code>
	 * OR logic:
	 * <code>
	 * $q->where('or', array('name', 'Tinkerbell'))
	 * </code>
	 * OR logic with operator
	 * <code>
	 * ->where('or', array('id', '>', 3))
	 * </code>
	 * Arrays represent brackets. e.g
	 * <code>
	 * $q->where('name', 'Tinkerbell')
	 *   ->where('or', array(
	 *        array('id', '>', 7),
	 *        array('id', '<', 15)
	 *   );
	 * //Will produce "WHERE `name`='Tinkerbell' OR (`id` > 7 AND `id` < 15)"
	 * </code>
	 * Multiple calls to where() append new conditions to previous ones
	 *
	 * @param mixed $column Column name, logic parameter 'OR' or 'AND' or an array of conditions
	 * @param mixed $operator Condition value, operator or an array of parameters
	 * @param mixed $val Condition value
	 *
	 * @return Query_Database  Returns self
	 */
	public function where()
	{
		$p = func_get_args();
		$cond = $this->get_condition_part($p);
		$this->_conditions = array_merge($this->_conditions, array($cond));

		return $this;
	}

	/**
	 * Recursively builds condition arrays for methods like where(), having()
	 *
	 * @param array     $p Parameters passed to the method
	 * @return array     Array in condition format
	 * @throws \Exception If condition format is incorrect
	 */
	private function get_condition_part($p)
	{
		if (is_string($p[0]) && (strtolower($p[0]) == 'or' || strtolower($p[0]) == 'and') && isset($p[1]) && is_array($p[1]))
		{
			$cond = $this->get_condition_part($p[1]);
			$cond['logic'] = strtolower($p[0]);
			return $cond;
		}

		if (is_array($p[0]))
		{
			if (count($p) == 1)
			{
				return $this->get_condition_part($p[0]);
			}
			$conds = array();
			foreach ($p as $q)
			{
				$conds[] = $this->get_condition_part($q);
			}
			if (count($conds) == 1)
			{
				return $conds;
			}
			return array('logic' => 'and', 'conditions' => $conds);
		}

		if ((is_string($p[0]) || $p[0] instanceof \PHPixie\DB\Expression) && isset($p[1]))
		{
			if (is_string($p[0]) && strpos($p[0], '.') === false)
			{
				$p[0] = $this->last_alias().'.'.$p[0];
			}
			return array(
				'logic' => 'and',
				'conditions' => array(
					'field' => $p[0],
					'operator' => isset($p[2]) ? $p[1] : '=',
					'value' => isset($p[2]) ? $p[2] : $p[1]
				)
			);
		}

		throw new \Exception('Incorrect conditional statement passed');
	}

	/**
	 * Adds a UNION to the query
	 * 
	 * @param  Query_Database|Expression_Database  $query Query for the UNION
	 * @param  string $all whether to do a UNION ALL, e.g. keep duplicate rows
	 * @return Query_Database  Returns self
	 */
	public function union($query,$all=true) {
		$this->_union[] = array($query,$all);
		return $this;
	}
	
	/**
	 * Gets last generated alias
	 *
	 * @return string  Last generated alias. If no alias were created returns table name.
	 */
	public function last_alias()
	{
		if ($this->_alias === null)
		{
			if (is_array($this->_table))
				return $this->_table[1];

			return $this->_table;
		}
		return 'a'.$this->_alias;
	}

	/**
	 * Generates new alias. Useful for dynamically adding aliases to joins.
	 * Alias is just a letter 'a' with an incremented number.
	 *
	 * @return string New alias
	 */
	public function add_alias()
	{
		if ($this->_alias === null)
		{
			$this->_alias = 0;
		}
		else
		{
			$this->_alias++;
		}
		return $this->last_alias();
	}

}
