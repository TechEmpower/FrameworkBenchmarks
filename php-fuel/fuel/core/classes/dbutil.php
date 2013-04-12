<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;

/**
 * DBUtil Class
 *
 * @package		Fuel
 * @category	Core
 * @author		Dan Horrigan
 */
class DBUtil
{

	/**
	 * @var  string  $connection  the database connection (identifier)
	 */
	protected static $connection = null;

	/**
	 * Sets the database connection to use for following DBUtil calls.
	 *
	 * @param  string|object  string connection name or \Database_Connection object, null for default
	 */
	public static function set_connection($connection, $db = null)
	{
		if ( ! is_string($connection) and ($connection instanceof Database_Connection))
		{
			throw new \FuelException('A connection must be supplied as a string or a Database_Connection object.');
		}

		static::$connection = $connection;
	}

	/**
	 * Creates a database.  Will throw a Database_Exception if it cannot.
	 *
	 * @throws	Fuel\Database_Exception
	 * @param	string	$database	the database name
	 * @param	string	$database	the character set
	 * @param	boolean	$if_not_exists  whether to add an IF NOT EXISTS statement.
	 * @return	int		the number of affected rows
	 */
	public static function create_database($database, $charset = null, $if_not_exists = true, $db = null)
	{
		$sql = 'CREATE DATABASE';
		$sql .= $if_not_exists ? ' IF NOT EXISTS ' : ' ';

		$charset = static::process_charset($charset, true);

		return \DB::query($sql.\DB::quote_identifier($database, $db ? $db : static::$connection).$charset, \DB::UPDATE)->execute($db ? $db : static::$connection);
	}

	/**
	 * Drops a database.  Will throw a Database_Exception if it cannot.
	 *
	 * @throws	Fuel\Database_Exception
	 * @param	string	$database	the database name
	 * @return	int		the number of affected rows
	 */
	public static function drop_database($database, $db = null)
	{
		return \DB::query('DROP DATABASE '.\DB::quote_identifier($database, $db ? $db : static::$connection), \DB::DELETE)->execute($db ? $db : static::$connection);
	}

	/**
	 * Drops a table.  Will throw a Database_Exception if it cannot.
	 *
	 * @throws	Fuel\Database_Exception
	 * @param	string	$table	the table name
	 * @return	int		the number of affected rows
	 */
	public static function drop_table($table, $db = null)
	{
		return \DB::query('DROP TABLE IF EXISTS '.\DB::quote_identifier(\DB::table_prefix($table, $db ? $db : static::$connection), $db ? $db : static::$connection), \DB::DELETE)->execute($db ? $db : static::$connection);
	}

	/**
	 * Renames a table.  Will throw a Database_Exception if it cannot.
	 *
	 * @throws	\Database_Exception
	 * @param	string	$table			the old table name
	 * @param	string	$new_table_name	the new table name
	 * @return	int		the number of affected
	 */
	public static function rename_table($table, $new_table_name, $db = null)
	{
		return \DB::query('RENAME TABLE '.\DB::quote_identifier(\DB::table_prefix($table, $db ? $db : static::$connection), $db ? $db : static::$connection).' TO '.\DB::quote_identifier(\DB::table_prefix($new_table_name)),\DB::UPDATE)->execute($db ? $db : static::$connection);
	}

	/**
	 * Creates a table.
	 *
	 * @throws	 \Database_Exception
	 * @param    string    $table          the table name
	 * @param    array     $fields         the fields array
	 * @param    array     $primary_keys   an array of primary keys
	 * @param    boolean   $if_not_exists  whether to add an IF NOT EXISTS statement.
	 * @param    string    $engine         storage engine overwrite
	 * @param    string    $charset        default charset overwrite
	 * @param    array     $foreign_keys   an array of foreign keys
	 * @return   int       number of affected rows.
	 */
	public static function create_table($table, $fields, $primary_keys = array(), $if_not_exists = true, $engine = false, $charset = null, $foreign_keys = array(), $db = null)
	{
		$sql = 'CREATE TABLE';

		$sql .= $if_not_exists ? ' IF NOT EXISTS ' : ' ';

		$sql .= \DB::quote_identifier(\DB::table_prefix($table, $db ? $db : static::$connection), $db ? $db : static::$connection).' (';
		$sql .= static::process_fields($fields);
		if ( ! empty($primary_keys))
		{
			$key_name = \DB::quote_identifier(implode('_', $primary_keys), $db ? $db : static::$connection);
			$primary_keys = \DB::quote_identifier($primary_keys, $db ? $db : static::$connection);
			$sql .= ",\n\tPRIMARY KEY ".$key_name." (" . implode(', ', $primary_keys) . ")";
		}

		empty($foreign_keys) or $sql .= static::process_foreign_keys($foreign_keys);

		$sql .= "\n)";
		$sql .= ($engine !== false) ? ' ENGINE = '.$engine.' ' : '';
		$sql .= static::process_charset($charset, true).";";

		return \DB::query($sql, \DB::UPDATE)->execute($db ? $db : static::$connection);
	}

	/**
	 * Adds fields to a table a table.  Will throw a Database_Exception if it cannot.
	 *
	 * @throws	Fuel\Database_Exception
	 * @param	string	$table			the table name
	 * @param	array	$fields			the new fields
	 * @return	int		the number of affected
	 */
	public static function add_fields($table, $fields, $db = null)
	{
		return static::alter_fields('ADD', $table, $fields, $db);
	}

	/**
	 * Modifies fields in a table.  Will throw a Database_Exception if it cannot.
	 *
	 * @throws	Fuel\Database_Exception
	 * @param	string	$table			the table name
	 * @param	array	$fields			the modified fields
	 * @return	int		the number of affected
	 */
	public static function modify_fields($table, $fields, $db = null)
	{
		return static::alter_fields('MODIFY', $table, $fields, $db);
	}

	/**
	 * Drops fields from a table a table.  Will throw a Database_Exception if it cannot.
	 *
	 * @throws	Fuel\Database_Exception
	 * @param	string			$table			the table name
	 * @param	string|array	$fields			the fields
	 * @return	int				the number of affected
	 */
	public static function drop_fields($table, $fields, $db = null)
	{
		return static::alter_fields('DROP', $table, $fields, $db);
	}

	protected static function alter_fields($type, $table, $fields, $db = null)
	{
		$sql = 'ALTER TABLE '.\DB::quote_identifier(\DB::table_prefix($table, $db ?: static::$connection), $db ?: static::$connection).' ';

		if ($type === 'DROP')
		{
			if ( ! is_array($fields))
			{
				$fields = array($fields);
			}

			$drop_fields = array();
			foreach ($fields as $field)
			{
				$drop_fields[] = 'DROP '.\DB::quote_identifier($field, $db ?: static::$connection);
			}
			$sql .= implode(', ', $drop_fields);
		}
		else
		{
			$use_brackets = ! in_array($type, array('ADD', 'CHANGE', 'MODIFY'));
			$use_brackets and $sql .= $type.' ';
			$use_brackets and $sql .= '(';
			$sql .= static::process_fields($fields, (( ! $use_brackets) ? $type.' ' : ''));
			$use_brackets and $sql .= ')';
		}

		return \DB::query($sql, \DB::UPDATE)->execute($db ?: static::$connection);
	}

	/**
	 * Creates an index on that table.
	 *
	 * @access	public
	 * @static
	 * @param	string	$table
	 * @param	string	$index_name
	 * @param	string	$index_columns
	 * @param	string	$index (should be 'unique', 'fulltext', 'spatial' or 'nonclustered')
	 * @return	bool
	 * @author	Thomas Edwards
	 */
	public static function create_index($table, $index_columns, $index_name = '', $index = '', $db = null)
	{
		static $accepted_index = array('UNIQUE', 'FULLTEXT', 'SPATIAL', 'NONCLUSTERED');

		// make sure the index type is uppercase
		$index !== '' and $index = strtoupper($index);

		if (empty($index_name))
		{
			if (is_array($index_columns))
			{
				foreach ($index_columns as $key => $value)
				{
					if (is_numeric($key))
					{
						$index_name .= ($index_name == '' ? '' : '_').$value;
					}
					else
					{
						$index_name .= ($index_name == '' ? '' : '_').str_replace(array('(', ')', ' '), '', $key);
					}
				}
			}
			else
			{
				$index_name = $index_columns;
			}
		}

		$sql = 'CREATE ';

		$index !== '' and $sql .= (in_array($index, $accepted_index)) ? $index.' ' : '';

		$sql .= 'INDEX ';
		$sql .= \DB::quote_identifier($index_name, $db ? $db : static::$connection);
		$sql .= ' ON ';
		$sql .= \DB::quote_identifier(\DB::table_prefix($table, $db ? $db : static::$connection), $db ? $db : static::$connection);
		if (is_array($index_columns))
		{
			$columns = '';
			foreach ($index_columns as $key => $value)
			{
				if (is_numeric($key))
				{
					$columns .= ($columns=='' ? '' : ', ').\DB::quote_identifier($value, $db ? $db : static::$connection);
				}
				else
				{
					$columns .= ($columns=='' ? '' : ', ').\DB::quote_identifier($key, $db ? $db : static::$connection).' '.strtoupper($value);
				}
			}
			$sql .= ' ('.$columns.')';
		}
		else
		{
			$sql .= ' ('.\DB::quote_identifier($index_columns, $db ? $db : static::$connection).')';
		}

		return \DB::query($sql, \DB::UPDATE)->execute($db ? $db : static::$connection);
	}

	/**
	 * Drop an index from a table.
	 *
	 * @access	public
	 * @static
	 * @param	string $table
	 * @param	string $index_name
	 * @return	bool
	 * @author	Thomas Edwards
	 */
	public static function drop_index($table, $index_name, $db = null)
	{
		$sql = 'DROP INDEX '.\DB::quote_identifier($index_name, $db ? $db : static::$connection);
		$sql .= ' ON '.\DB::quote_identifier(\DB::table_prefix($table, $db ? $db : static::$connection), $db ? $db : static::$connection);

		return \DB::query($sql, \DB::UPDATE)->execute($db ? $db : static::$connection);
	}

	protected static function process_fields($fields, $prefix = '', $db = null)
	{
		$sql_fields = array();

		foreach ($fields as $field => $attr)
		{
			$attr = array_change_key_case($attr, CASE_UPPER);
			$_prefix = $prefix;
			if(array_key_exists('NAME', $attr) and $field !== $attr['NAME'] and $_prefix === 'MODIFY ')
			{
				$_prefix = 'CHANGE ';
			}
			$sql = "\n\t".$_prefix;
			$sql .= \DB::quote_identifier($field);
			$sql .= (array_key_exists('NAME', $attr) and $attr['NAME'] !== $field) ? ' '.\DB::quote_identifier($attr['NAME'], $db ? $db : static::$connection).' ' : '';
			$sql .= array_key_exists('TYPE', $attr) ? ' '.$attr['TYPE'] : '';

			if(array_key_exists('CONSTRAINT',$attr))
			{
				if(is_array($attr['CONSTRAINT']))
				{
					$sql .= "('".implode("', '",$attr['CONSTRAINT'])."')";
				}
				else
				{
					$sql .= '('.$attr['CONSTRAINT'].')';
				}
			}

			$sql .= array_key_exists('CHARSET', $attr) ? static::process_charset($attr['CHARSET'], $db ? $db : static::$connection) : '';

			if (array_key_exists('UNSIGNED', $attr) and $attr['UNSIGNED'] === true)
			{
				$sql .= ' UNSIGNED';
			}

			if(array_key_exists('DEFAULT', $attr))
			{
				$sql .= ' DEFAULT '.(($attr['DEFAULT'] instanceof \Database_Expression) ? $attr['DEFAULT']  : \DB::escape($attr['DEFAULT']));
			}

			if(array_key_exists('NULL', $attr) and $attr['NULL'] === true)
			{
				$sql .= ' NULL';
			}
			else
			{
				$sql .= ' NOT NULL';
			}

			if (array_key_exists('AUTO_INCREMENT', $attr) and $attr['AUTO_INCREMENT'] === true)
			{
				$sql .= ' AUTO_INCREMENT';
			}

			if (array_key_exists('FIRST', $attr) and $attr['FIRST'] === true)
			{
				$sql .= ' FIRST';
			}
			elseif (array_key_exists('AFTER', $attr) and strval($attr['AFTER']))
			{
				$sql .= ' AFTER '.\DB::quote_identifier($attr['AFTER'], $db ? $db : static::$connection);
			}

			if (array_key_exists('COMMENT', $attr))
			{
				$sql .= ' COMMENT '.\DB::escape($attr['COMMENT'], $db ? $db : static::$connection);
			}

			$sql_fields[] = $sql;
		}

		return \implode(',', $sql_fields);
	}

	/**
	 * Formats the default charset.
	 *
	 * @param    string    $charset       the character set
	 * @param    bool      $is_default    whether to use default
	 * @return   string    the formated charset sql
	 */
	protected static function process_charset($charset = null, $is_default = false, $db = null)
	{
		$charset or $charset = \Config::get('db.'.($db ? $db : \Config::get('db.active')).'.charset', null);
		if (empty($charset))
		{
			return '';
		}

		if (($pos = stripos($charset, '_')) !== false)
		{
			$charset = ' CHARACTER SET '.substr($charset, 0, $pos).' COLLATE '.$charset;
		}
		else
		{
			$charset = ' CHARACTER SET '.$charset;
		}

		$is_default and $charset = ' DEFAULT'.$charset;

		return $charset;
	}

	/**
	 * Adds a single foreign key to a table
	 *
	 * @param	string	$table			the table name
	 * @param	array 	$foreign_key	a single foreign key
	 * @return 	int		number of affected rows
	 */
	public static function add_foreign_key($table, $foreign_key)
	{
		if ( ! is_array($foreign_key))
		{
			throw new InvalidArgumentException('Foreign key for add_foreign_key() must be specified as an array');
		}

		$sql = 'ALTER TABLE ';
		$sql .= \DB::quote_identifier(\DB::table_prefix($table)).' ';
		$sql .= 'ADD ';
		$sql .= ltrim(static::process_foreign_keys(array($foreign_key)), ',');

		return \DB::query($sql, \DB::UPDATE)->execute();
	}

	/**
	 * Drops a foreign key from a table
	 *
	 * @param	string	$table		the table name
	 * @param	string	$fk_name	the foreign key name
	 * @return 	int		number of affected rows
	 */
	public static function drop_foreign_key($table, $fk_name)
	{
		$sql = 'ALTER TABLE ';
		$sql .= \DB::quote_identifier(\DB::table_prefix($table)).' ';
		$sql .= 'DROP FOREIGN KEY '.\DB::quote_identifier($fk_name);

		return \DB::query($sql, \DB::UPDATE)->execute();
	}


	/**
	 * Returns string of foreign keys
	 *
	 * @param    array    $foreign_keys       Array of foreign key rules
	 * @return   string    the formated foreign key string
	 */
	public static function process_foreign_keys($foreign_keys, $db = null)
	{
		if ( ! is_array($foreign_keys))
		{
			throw new \Database_Exception('Foreign keys on create_table() must be specified as an array');
		}

		$fk_list = array();

		foreach($foreign_keys as $definition)
		{
			// some sanity checks
			if (empty($definition['key']))
			{
				throw new \Database_Exception('Foreign keys on create_table() must specify a foreign key name');
			}
			if ( empty($definition['reference']))
			{
				throw new \Database_Exception('Foreign keys on create_table() must specify a foreign key reference');
			}
			if (empty($definition['reference']['table']) or empty($definition['reference']['column']))
			{
				throw new \Database_Exception('Foreign keys on create_table() must specify a reference table and column name');
			}

			$sql = '';
			! empty($definition['constraint']) and $sql .= " CONSTRAINT ".\DB::quote_identifier($definition['constraint']);
			$sql .= " FOREIGN KEY (".\DB::quote_identifier($definition['key']).')';
			$sql .= " REFERENCES ".\DB::quote_identifier(\DB::table_prefix($definition['reference']['table'])).' (';
			if (is_array($definition['reference']['column']))
			{
				$sql .= implode(', ', \DB::quote_identifier($definition['reference']['column']));
			}
			else
			{
				$sql .= \DB::quote_identifier($definition['reference']['column']);
			}
			$sql .= ')';
			! empty($definition['on_update']) and $sql .= " ON UPDATE ".$definition['on_update'];
			! empty($definition['on_delete']) and $sql .= " ON DELETE ".$definition['on_delete'];

			$fk_list[] = "\n\t".ltrim($sql);
		}

		return ', '.implode(',', $fk_list);
	}

	/**
	 * Truncates a table.
	 *
	 * @throws    Fuel\Database_Exception
	 * @param     string    $table    the table name
	 * @return    int       the number of affected rows
	 */
	public static function truncate_table($table, $db = null)
	{
		return \DB::query('TRUNCATE TABLE '.\DB::quote_identifier(\DB::table_prefix($table, $db ? $db : static::$connection), $db ? $db : static::$connection), \DB::DELETE)
			->execute($db ? $db : static::$connection);
	}

	/**
	 * Analyzes a table.
	 *
	 * @param     string    $table    the table name
	 * @return    bool      whether the table is OK
	 */
	public static function analyze_table($table, $db = null)
	{
		return static::table_maintenance('ANALYZE TABLE', $table, $db);
	}

	/**
	 * Checks a table.
	 *
	 * @param     string    $table    the table name
	 * @return    bool      whether the table is OK
	 */
	public static function check_table($table, $db = null)
	{
		return static::table_maintenance('CHECK TABLE', $table, $db);
	}

	/**
	 * Optimizes a table.
	 *
	 * @param     string    $table    the table name
	 * @return    bool      whether the table has been optimized
	 */
	public static function optimize_table($table, $db = null)
	{
		return static::table_maintenance('OPTIMIZE TABLE', $table, $db);
	}

	/**
	 * Repairs a table.
	 *
	 * @param     string    $table    the table name
	 * @return    bool      whether the table has been repaired
	 */
	public static function repair_table($table, $db = null)
	{
		return static::table_maintenance('REPAIR TABLE', $table, $db);
	}

	/**
	 * Checks if a given table exists.
	 *
	 * @param   string  $table  Table name
	 * @return  bool
	 */
	public static function table_exists($table, $db = null)
	{
		try
		{
			\DB::select()->from($table)->limit(1)->execute($db ? $db : static::$connection);
			return true;
		}
		catch (\Database_Exception $e)
		{
			// check if we have a DB connection at all
			$connection = \Database_Connection::instance($db ? $db : static::$connection)->connection();

			// if no connection could be made, re throw the exception
			if ( ! $connection)
			{
				throw $e;
			}

			return false;
		}
	}

	/**
	 * Checks if given field(s) in a given table exists.
	 *
	 * @param   string         $table    Table name
	 * @param   string|array   $columns  columns to check
	 * @return  bool
	 */
	public static function field_exists($table, $columns, $db = null)
	{
		if ( ! is_array($columns))
		{
			$columns = array($columns);
		}

		try
		{
			\DB::select_array($columns)->from($table)->limit(1)->execute($db ? $db : static::$connection);
			return true;
		}
		catch (\Database_Exception $e)
		{
			return false;
		}
	}

	/*
	 * Executes table maintenance. Will throw FuelException when the operation is not supported.
	 *
	 * @throws	FuelException
	 * @param     string    $table    the table name
	 * @return    bool      whether the operation has succeeded
	 */
	protected static function table_maintenance($operation, $table, $db = null)
	{
		$result = \DB::query($operation.' '.\DB::quote_identifier(\DB::table_prefix($table, $db ? $db : static::$connection), $db ? $db : static::$connection), \DB::SELECT)->execute($db ? $db : static::$connection);
		$type = $result->get('Msg_type');
		$message = $result->get('Msg_text');
		$table = $result->get('Table');
		if ($type === 'status' and in_array(strtolower($message), array('ok','table is already up to date')))
		{
			return true;
		}

		if ($type === 'error')
		{
			logger(\Fuel::L_ERROR, 'Table: '.$table.', Operation: '.$operation.', Message: '.$result->get('Msg_text'), 'DBUtil::table_maintenance');
		}
		else
		{
			logger(ucfirst($type), 'Table: '.$table.', Operation: '.$operation.', Message: '.$result->get('Msg_text'), 'DBUtil::table_maintenance');
		}
		return false;
	}

	/*
	 * Load the db config, the Database_Connection might not have fired jet.
	 *
	 */
	public static function _init()
	{
		\Config::load('db', true);
	}

}

