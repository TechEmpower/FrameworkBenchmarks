<?php
/**
 * PostgreSQL Migration
 *
 * Migration class for the PostgreSQL database.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro\Migration;

class PGSQL extends \Micro\Migration
{

	// Backup all existing data
	public function backup_data()
	{
		if( ! $this->tables) die('No tables given');

		$tables = array();

		$sql = "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'";

		// Build list of all tables
		foreach($this->db->fetch($sql) as $row) $tables[] = current($row);

		if($tables)
		{
			$this->backup_tables($tables);
		}
	}

	// Drop database schema
	public function drop_schema()
	{
		//
	}

	/**
	 * Create database schema
	 */
	public function create_schema()
	{
		if( ! $this->tables) die('No tables given');

		// Create each table
		foreach($this->tables as $table => $schema)
		{
			// Report status to user
			print 'Dropping table '. colorize($table, 'yellow')."\n";

			// Remove table
			$this->db->query("DROP TABLE IF EXISTS \"$table\"");

			$sql = "CREATE TABLE \"$table\" (\n";

			$index = array();
			$unique = array();

			// Defaults for columns
			$defaults = array(
				//'type' => 'primary|string|integer|boolean|decimal|datetime', REQUIRED!
				'type' => 'string',
				'length' => NULL,
				'index' => FALSE,
				'null' => TRUE,
				'default' => '',
				'unique' => FALSE,
				'precision' => 0,
				'scale' => 0,
			);

			foreach($schema as $column => $data)
			{
				$data = $data + $defaults;

				$type = $data['type'];

				// Integer?
				if($type == 'primary' OR $type == 'integer')
				{
					// Default to int
					$length = $data['length'] ? $data['length'] : 2147483647;

					if($length <= 32767)
						$type = 'smallint';
					elseif($length <= 2147483647)
						$type = 'integer';
					else
						$type = 'bigint';

					// Is this the primary column?
					if($data['type'] == 'primary')
					{
						$primary = $column;

						// Primary keys are special
						$sql .= "\t\"$column\" serial primary key,\n";
						continue;
					}
				}
				elseif($type == 'string')
				{
					// Even if "text" isn't a valid type in SQL
					// PostgreSQL treats it the same as "character varying" (i.e. "varchar")
					$type = 'text';
				}
				elseif($type == 'boolean')
				{
					$type = 'boolean';
				}
				elseif($type == 'decimal')
				{
					$type = 'decimal('. $data['precision'].','. $data['scale'].')';
				}
				else
				{
					$type = 'timestamp without time zone';
				}

				// Build Column Definition
				$sql .= "\t\"$column\" $type";

				// NULL and FALSE are both valid defaults
				if($data['default'] !== '')
				{
					if(is_bool($data['default']) OR $data['default'] === NULL)
					{
						$sql .= ' DEFAULT '. $data['default'];
					}
					else
					{
						$sql .= ' DEFAULT \''. $data['default']. "'";
					}
				}

				// Add NULL
				if(! $data['null']) $sql .= ' NOT NULL';

				$sql .= ",\n";

				// Is the column unique?
				if($data['unique']) $unique[] = $column;

				// Index the column?
				if($data['index']) $index[] = $column;
			}

			foreach($unique as $column)
			{
				$key = $table.'_'. $column.'_u';//.chr(mt_rand(65,90));

				$sql .= "CONSTRAINT $key UNIQUE ($column),\n";

				// Creating a unique constraint automattically creates an index
				foreach($index as $id => $field)
				{
					if($field === $column)
					{
						unset($index[$id]);
					}
				}
			}

			// Remove ending comma and close table
			$sql = substr($sql,0,-2)."\n);";

			// Create table
			print $sql."\n";
			$this->db->query($sql);

			// Create any indexes
			foreach($index as $column)
			{
				$key = $table.'_'. $column.'_i';//.chr(mt_rand(65,90));
				$sql = "CREATE INDEX $key ON \"$table\" USING btree ($column)";

				print $sql."\n";
				$this->db->query($sql);
			}

			// Report status to user
			print 'Created table '. colorize($table, 'green')."\n\n";

		}

		print colorize('Schema Created', 'blue')."\n\n";
	}

}
