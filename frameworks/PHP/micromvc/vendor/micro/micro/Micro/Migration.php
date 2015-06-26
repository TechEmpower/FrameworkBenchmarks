<?php
/**
 * Migration
 *
 * Base migration class to be extended by the Database-Specific wrapper.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

abstract class Migration
{
	public $tables;
	public $db;
	public $name;

	// Backup all existing data
	abstract public function backup_data();

	// Drop database schema
	abstract public function drop_schema();

	// Create database schema
	abstract public function create_schema();

	// Insert backed-up data into new database schema
	public function restore_data()
	{
		if( ! $this->tables) die('No tables given');

		$file = $this->backup_path().$this->name.'_current_backup.json';

		if(!is_file($file))
		{
			// Report status to user
			print 'Backup file not found ('. colorize($file, 'yellow').")\n";

			return;
		}

		$handle = fopen($file, "r");

		//if(empty($tables)) die(colorize('Cannot restore backup, invalid JSON data', 'red')."\n");
		if( ! $handle) die(colorize('Cannot open backup file', 'red')."\n");

		try
		{
			// Start transaction
			$this->db->pdo->beginTransaction();

			$table = NULL;
			$columns = array();

			/*
			while (!feof($handle))
			{
				$line = fread($handle, 8192);
			*/
			while (($line = fgets($handle)) !== false)
			{
				$line = rtrim($line);

				// Table name
				if($line{0} !== '{')
				{
					$table = $line;

					// Has this table been removed from the schema?
					if( ! isset($this->tables[$table]) )
					{
						print colorize("$table no longer exists in schema, ignoring",'yellow')."\n";
						$table = NULL;
					}
					else
					{
						// Column list comes from new schema - not old backup
						$columns = array_flip(array_keys($this->tables[$table]));
						print colorize("Restoring $table...", 'green')."\n";
					}

					continue;
				}

				if( ! $table) continue; // Current table is being ignored

				// Decode JSON row object
				$line = (array) json_decode($line);

				/*
				 * Some databases (like PostgreSQL) cannot handle incorrect FALSE values.
				 * For example, PostgreSQL CANNOT handle empty ("") values in integer columns.
				 *
				 * So, we will simply remove all empty values from the insert giving
				 * them a default of NULL or EMPTY as the database decides.
				 */
				foreach($line as $key => $value)
				{
					//if( ! $value AND $value !== NULL) unset($line[$key]);
					if($value === '') unset($line[$key]);
				}

				// Insert row *only* taking schema columns into account
				$this->db->insert($table, array_intersect_key((array) $line, $columns));
			}

			// Commit Transaction
			$this->db->pdo->commit();

			print colorize('Finished Restoring Data', 'blue'). "\n\n";
		}
		catch(PDOException $e)
		{
			// Roolback changes (all or nothing)
			$this->db->pdo->rollBack();

			fclose($handle);

			die(colorize($e->getMessage(), 'red')."\n");
		}

		fclose($handle);
	}

	// Path to backup files
	protected function backup_path()
	{
		return SP . 'App/Backups/';
	}

	// Backup all existing data
	protected function backup_tables($tables)
	{
		if( ! $this->tables) die('No tables given');

		// Build path to backup directory
		$file =  $this->backup_path(). get_class($this). '.'. $this->name. '.'.date("Y.m.d_H:i").'.json';

		// Open the backup file for writing and truncate it
		$handle = fopen($file, 'w');

		// Does anything actually get backed-up?
		$found = FALSE;

		// Backup all data in this schema
		foreach($this->tables as $table => $schema)
		{
			// Don't try to back it up if it doesn't exist
			if( ! in_array($table, $tables))
			{
				// Report status to user
				print 'Skipping '. colorize($table, 'yellow')."\n";
				continue;
			}

			$found = TRUE;

			// Start of new table
			fwrite($handle, $table. "\n");

			// Report status to user
			print 'Backing up '. colorize($table, 'green')."\n";

			// Fetch all records
			$statement = $this->db->query('SELECT * FROM '. $this->db->i. $table. $this->db->i);

			// We want named keys
			$statement->setFetchMode(\PDO::FETCH_ASSOC);

			// Write each record one at a time to save memory
			foreach($statement as $row)
			{
				fwrite($handle, json_encode($row)."\n");
			}
		}

		// We're done here
		fclose($handle);

		if($found)
		{
			// Make this file the new masterbackup
			copy($file, $this->backup_path() . $this->name . '_current_backup.json');

			// Report status to user
			print 'Backup saved to '. colorize($file, 'blue')."\n\n";
		}
		else
		{
			print colorize('Nothing to backup', 'yellow')."\n";
		}
	}
}
