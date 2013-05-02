<?php
/**
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Oil;

/**
 * Oil\Generate_Migration_Actions
 * Handles actions for generating migrations in Oil
 *
 * @package		Fuel
 * @subpackage	Oil
 * @category	Core
 * @author		Tom Arnfeld
 */
class Generate_Migration_Actions
{

	/**
	 * Each migration action should return an array with two items, 0 being the up and 1 the being down.
	 */

	// create_{tablename}
	public static function create($subjects, $fields)
	{
		$field_str = '';
		$defined_columns = array();
		$have_id = false;

		foreach($fields as $field)
		{
			$name = array_shift($field);

			$name === 'id' and $have_id = true;

			$field_opts = array();
			foreach($field as $option => $val)
			{
				if($val === true)
				{
					$field_opts[] = "'$option' => true";
				}
				else
				{
					if(is_int($val))
					{
						$field_opts[] = "'$option' => $val";
					}
					else
					{
						$field_opts[] = "'$option' => '$val'";
					}
				}
			}
			$field_opts = implode(', ', $field_opts);

			$field_str .= "\t\t\t'$name' => array({$field_opts}),".PHP_EOL;
			$defined_columns[$name] = true;
		}

		// ID Field
		$have_id or $field_str = "\t\t\t'id' => array('constraint' => 11, 'type' => 'int', 'auto_increment' => true),".PHP_EOL . $field_str;

		$up = <<<UP
		\DBUtil::create_table('{$subjects[1]}', array(
$field_str
		), array('id'));
UP;

		$down = <<<DOWN
		\DBUtil::drop_table('{$subjects[1]}');
DOWN;

		return array($up, $down);
	}

	// add_{thing}_to_{tablename}
	public static function add($subjects, $fields, $reverse = false)
	{
		$field_up_str = '';

		foreach($fields as $field)
		{
			$name = array_shift($field);

			$field_opts = array();

			foreach($field as $option => $val)
			{
				if($val === true)
				{
					$field_opts[] = "'$option' => true";
				}
				else
				{
					if(is_int($val))
					{
						$field_opts[] = "'$option' => $val";
					}
					else
					{
						$field_opts[] = "'$option' => '$val'";
					}
				}
			}

			$field_opts = implode(', ', $field_opts);

			$field_up_str .= "\t\t\t'$name' => array({$field_opts}),".PHP_EOL;
			$field_down[] = "\t\t\t'$name'".PHP_EOL;
		}

		$field_down_str = implode(',', $field_down);

		$up = <<<UP
		\DBUtil::add_fields('{$subjects[1]}', array(
$field_up_str
		));
UP;
		$down = <<<DOWN
		\DBUtil::drop_fields('{$subjects[1]}', array(
$field_down_str
		));
DOWN;
		return $reverse ? array($down, $up) : array($up, $down);
	}

	// delete_{thing}_from_{tablename}
	public static function delete($subjects, $fields, $reverse = false)
	{
		return static::add($subjects, $fields, true);
	}

	// rename_field_{fieldname}_to_{newfieldname}_in_{table}
	public static function rename_field($subjects, $fields)
	{
		$column_list = \DB::list_columns($subjects[0], $subjects[1]);
		$column = $column_list[$subjects[1]];

		switch ($column['type'])
		{
			case 'float':
				$constraint = '\''.$column['numeric_precision'].', '.$column['numeric_scale'].'\'';
			break;
			case 'int':
				$constraint = $column['display'];
			break;
			case 'string':
				switch ($column['data_type'])
				{
					case 'binary':
					case 'varbinary':
					case 'char':
					case 'varchar':
						$constraint = $column['character_maximum_length'];
					break;

					case 'enum':
					case 'set':
					  $constraint = '"\''.implode('\',\'',$column['options']).'\'"';
					break;
				}
			break;
		}

		$constraint_str = isset($constraint) ? ", 'constraint' => $constraint" : '';

		$up = <<<UP
		\DBUtil::modify_fields('{$subjects[0]}', array(
\t\t\t'{$subjects[1]}' => array('name' => '{$subjects[2]}', 'type' => '{$column['data_type']}'$constraint_str)
		));
UP;
		$down = <<<DOWN
	\DBUtil::modify_fields('{$subjects[0]}', array(
\t\t\t'{$subjects[2]}' => array('name' => '{$subjects[1]}', 'type' => '{$column['data_type']}'$constraint_str)
		));
DOWN;
		return array($up, $down);
	}

	// rename_table_{tablename}_to_{newtablename}
	public static function rename_table($subjects, $fields)
	{

		$up = <<<UP
		\DBUtil::rename_table('{$subjects[0]}', '{$subjects[1]}');
UP;
		$down = <<<DOWN
		\DBUtil::rename_table('{$subjects[1]}', '{$subjects[0]}');
DOWN;

		return array($up, $down);
	}

	// drop_{tablename}
	public static function drop($subjects, $fields)
	{
		$up = <<<UP
		\DBUtil::drop_table('{$subjects[1]}');
UP;
		$field_str = '';
		$column_list = \DB::list_columns($subjects[1]);

		foreach ($column_list as $column)
		{
			switch ($column['type'])
			{
				case 'float':
					$constraint = '\''.$column['numeric_precision'].', '.$column['numeric_scale'].'\'';
				break;
				case 'int':
					$constraint = $column['display'];
				break;
				case 'string':
					switch ($column['data_type'])
					{
						case 'binary':
						case 'varbinary':
						case 'char':
						case 'varchar':
							$constraint = $column['character_maximum_length'];
						break;

						case 'enum':
						case 'set':
							$constraint = '"\''.implode('\',\'',$column['options']).'\'"';
						break;
					}
				break;
			}

			$constraint_str = isset($constraint) ? ", 'constraint' => $constraint" : '';
			$auto_increment = $column['extra'] == 'auto_increment' ? ", 'auto_increment' => true" : '';
			$default_str = $column['default'] != null ? ", 'default' => '{$column['default']}'" : ", 'null' => true";

			if ($column['key'] == 'PRI')
			{
			  $primary_keys[] = "'{$column['name']}'";
			}
			else if ($column['key'] == 'MUL')
			{
			  $indexes[] = $column['name'];
			}

			$field_str .= "\t\t\t'{$column['name']}' => array('type' => '{$column['data_type']}'{$default_str}{$constraint_str}{$auto_increment}),".PHP_EOL;
			unset($constraint);
		}

		$primary_keys = implode(',', $primary_keys);
		$down = <<<DOWN
		\DBUtil::create_table('{$subjects[1]}', array(
$field_str
		), array($primary_keys));
DOWN;
		$down .= PHP_EOL;

		$active_db = \Config::get('db.active');
		$table_prefix = \Config::get('db.'.$active_db.'.table_prefix');

		if (isset($indexes))
		{
			foreach ($indexes as $field)
			{
				$down .= "\t\t\\DB::query(\"CREATE INDEX {$field}_idx ON {$table_prefix}{$subjects[1]} (`{$field}`)\")->execute();".PHP_EOL;
			}
		}

		return array($up, $down);
	}

}
