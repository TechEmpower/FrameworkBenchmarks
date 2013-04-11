<?php namespace Laravel\Database\Schema\Grammars;

use Laravel\Fluent;
use Laravel\Database\Schema\Table;

abstract class Grammar extends \Laravel\Database\Grammar {

	/**
	 * Generate the SQL statement for creating a foreign key.
	 *
	 * @param  Table    $table
	 * @param  Fluent  $command
	 * @return string
	 */
	public function foreign(Table $table, Fluent $command)
	{
		$name = $command->name;

		// We need to wrap both of the table names in quoted identifiers to protect
		// against any possible keyword collisions, both the table on which the
		// command is being executed and the referenced table are wrapped.
		$table = $this->wrap($table);

		$on = $this->wrap_table($command->on);

		// Next we need to columnize both the command table's columns as well as
		// the columns referenced by the foreign key. We'll cast the referenced
		// columns to an array since they aren't by the fluent command.
		$foreign = $this->columnize($command->columns);

		$referenced = $this->columnize((array) $command->references);

		$sql = "ALTER TABLE $table ADD CONSTRAINT $name ";

		$sql .= "FOREIGN KEY ($foreign) REFERENCES $on ($referenced)";

		// Finally we will check for any "on delete" or "on update" options for
		// the foreign key. These control the behavior of the constraint when
		// an update or delete statement is run against the record.
		if ( ! is_null($command->on_delete))
		{
			$sql .= " ON DELETE {$command->on_delete}";
		}

		if ( ! is_null($command->on_update))
		{
			$sql .= " ON UPDATE {$command->on_update}";
		}

		return $sql;
	}

	/**
	 * Generate the SQL statement for a drop table command.
	 *
	 * @param  Table   $table
	 * @param  Fluent  $command
	 * @return string
	 */
	public function drop(Table $table, Fluent $command)
	{
		return 'DROP TABLE '.$this->wrap($table);
	}

	/**
	 * Drop a constraint from the table.
	 *
	 * @param  Table   $table
	 * @param  Fluent  $command
	 * @return string
	 */
	protected function drop_constraint(Table $table, Fluent $command)
	{
		return "ALTER TABLE ".$this->wrap($table)." DROP CONSTRAINT ".$command->name;
	}

	/**
	 * Wrap a value in keyword identifiers.
	 *
	 * @param  Table|string  $value
	 * @return string
	 */
	public function wrap($value)
	{
		// This method is primarily for convenience so we can just pass a
		// column or table instance into the wrap method without sending
		// in the name each time we need to wrap one of these objects.
		if ($value instanceof Table)
		{
			return $this->wrap_table($value->name);
		}
		elseif ($value instanceof Fluent)
		{
			$value = $value->name;
		}

		return parent::wrap($value);
	}

	/**
	 * Get the appropriate data type definition for the column.
	 *
	 * @param  Fluent  $column
	 * @return string
	 */
	protected function type(Fluent $column)
	{
		return $this->{'type_'.$column->type}($column);
	}

	/**
	 * Format a value so that it can be used in SQL DEFAULT clauses.
	 * @param  mixed   $value
	 * @return string
	 */
	protected function default_value($value)
	{
		if (is_bool($value))
		{
			return intval($value);
		}

		return strval($value);
	}

}