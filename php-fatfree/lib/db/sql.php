<?php

/*
	Copyright (c) 2009-2014 F3::Factory/Bong Cosca, All rights reserved.

	This file is part of the Fat-Free Framework (http://fatfree.sf.net).

	THE SOFTWARE AND DOCUMENTATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF
	ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
	PURPOSE.

	Please see the license.txt file for more information.
*/

namespace DB;

//! PDO wrapper
class SQL extends \PDO {

	protected
		//! UUID
		$uuid,
		//! Data source name
		$dsn,
		//! Database engine
		$engine,
		//! Database name
		$dbname,
		//! Transaction flag
		$trans=FALSE,
		//! Number of rows affected by query
		$rows=0,
		//! SQL log
		$log;

	/**
	*	Begin SQL transaction
	*	@return bool
	**/
	function begin() {
		$out=parent::begintransaction();
		$this->trans=TRUE;
		return $out;
	}

	/**
	*	Rollback SQL transaction
	*	@return bool
	**/
	function rollback() {
		$out=parent::rollback();
		$this->trans=FALSE;
		return $out;
	}

	/**
	*	Commit SQL transaction
	*	@return bool
	**/
	function commit() {
		$out=parent::commit();
		$this->trans=FALSE;
		return $out;
	}

	/**
	*	Map data type of argument to a PDO constant
	*	@return int
	*	@param $val scalar
	**/
	function type($val) {
		switch (gettype($val)) {
			case 'NULL':
				return \PDO::PARAM_NULL;
			case 'boolean':
				return \PDO::PARAM_BOOL;
			case 'integer':
				return \PDO::PARAM_INT;
			default:
				return \PDO::PARAM_STR;
		}
	}

	/**
	*	Cast value to PHP type
	*	@return scalar
	*	@param $type string
	*	@param $val scalar
	**/
	function value($type,$val) {
		switch ($type) {
			case \PDO::PARAM_NULL:
				return (unset)$val;
			case \PDO::PARAM_INT:
				return (int)$val;
			case \PDO::PARAM_BOOL:
				return (bool)$val;
			case \PDO::PARAM_STR:
				return (string)$val;
		}
	}

	/**
	*	Execute SQL statement(s)
	*	@return array|int|FALSE
	*	@param $cmds string|array
	*	@param $args string|array
	*	@param $ttl int
	*	@param $log bool
	**/
	function exec($cmds,$args=NULL,$ttl=0,$log=TRUE) {
		$auto=FALSE;
		if (is_null($args))
			$args=array();
		elseif (is_scalar($args))
			$args=array(1=>$args);
		if (is_array($cmds)) {
			if (count($args)<($count=count($cmds)))
				// Apply arguments to SQL commands
				$args=array_fill(0,$count,$args);
			if (!$this->trans) {
				$this->begin();
				$auto=TRUE;
			}
		}
		else {
			$cmds=array($cmds);
			$args=array($args);
		}
		$fw=\Base::instance();
		$cache=\Cache::instance();
		$result=FALSE;
		foreach (array_combine($cmds,$args) as $cmd=>$arg) {
			if (!preg_replace('/(^\s+|[\s;]+$)/','',$cmd))
				continue;
			$now=microtime(TRUE);
			$keys=$vals=array();
			if ($fw->get('CACHE') && $ttl && ($cached=$cache->exists(
				$hash=$fw->hash($this->dsn.$cmd.
				$fw->stringify($arg)).'.sql',$result)) &&
				$cached[0]+$ttl>microtime(TRUE)) {
				foreach ($arg as $key=>$val) {
					$vals[]=$fw->stringify(is_array($val)?$val[0]:$val);
					$keys[]='/'.(is_numeric($key)?'\?':preg_quote($key)).'/';
				}
			}
			elseif (is_object($query=$this->prepare($cmd))) {
				foreach ($arg as $key=>$val) {
					if (is_array($val)) {
						// User-specified data type
						$query->bindvalue($key,$val[0],$val[1]);
						$vals[]=$fw->stringify($this->value($val[1],$val[0]));
					}
					else {
						// Convert to PDO data type
						$query->bindvalue($key,$val,
							$type=$this->type($val));
						$vals[]=$fw->stringify($this->value($type,$val));
					}
					$keys[]='/'.(is_numeric($key)?'\?':preg_quote($key)).'/';
				}
				$query->execute();
				$error=$query->errorinfo();
				if ($error[0]!=\PDO::ERR_NONE) {
					// Statement-level error occurred
					if ($this->trans)
						$this->rollback();
					user_error('PDOStatement: '.$error[2]);
				}
				if (preg_match('/^\s*'.
					'(?:CALL|EXPLAIN|SELECT|PRAGMA|SHOW|RETURNING|EXEC)\b/is',
					$cmd)) {
					$result=$query->fetchall(\PDO::FETCH_ASSOC);
					// Work around SQLite quote bug
					if (preg_match('/sqlite2?/',$this->engine))
						foreach ($result as $pos=>$rec) {
							unset($result[$pos]);
							$result[$pos]=array();
							foreach ($rec as $key=>$val)
								$result[$pos][trim($key,'\'"[]`')]=$val;
						}
					$this->rows=count($result);
					if ($fw->get('CACHE') && $ttl)
						// Save to cache backend
						$cache->set($hash,$result,$ttl);
				}
				else
					$this->rows=$result=$query->rowcount();
				$query->closecursor();
				unset($query);
			}
			else {
				$error=$this->errorinfo();
				if ($error[0]!=\PDO::ERR_NONE) {
					// PDO-level error occurred
					if ($this->trans)
						$this->rollback();
					user_error('PDO: '.$error[2]);
				}
			}
			if ($log)
				$this->log.=date('r').' ('.
					sprintf('%.1f',1e3*(microtime(TRUE)-$now)).'ms) '.
					(empty($cached)?'':'[CACHED] ').
					preg_replace($keys,$vals,$cmd,1).PHP_EOL;
		}
		if ($this->trans && $auto)
			$this->commit();
		return $result;
	}

	/**
	*	Return number of rows affected by last query
	*	@return int
	**/
	function count() {
		return $this->rows;
	}

	/**
	*	Return SQL profiler results
	*	@return string
	**/
	function log() {
		return $this->log;
	}

	/**
	*	Retrieve schema of SQL table
	*	@return array|FALSE
	*	@param $table string
	*	@param $fields array|string
	*	@param $ttl int
	**/
	function schema($table,$fields=NULL,$ttl=0) {
		// Supported engines
		$cmd=array(
			'sqlite2?'=>array(
				'PRAGMA table_info("'.$table.'");',
				'name','type','dflt_value','notnull',0,'pk',TRUE),
			'mysql'=>array(
				'SHOW columns FROM `'.$this->dbname.'`.`'.$table.'`;',
				'Field','Type','Default','Null','YES','Key','PRI'),
			'mssql|sqlsrv|sybase|dblib|pgsql|odbc'=>array(
				'SELECT '.
					'c.column_name AS field,'.
					'c.data_type AS type,'.
					'c.column_default AS defval,'.
					'c.is_nullable AS nullable,'.
					't.constraint_type AS pkey '.
				'FROM information_schema.columns AS c '.
				'LEFT OUTER JOIN '.
					'information_schema.key_column_usage AS k '.
					'ON '.
						'c.table_name=k.table_name AND '.
						'c.column_name=k.column_name AND '.
						'c.table_schema=k.table_schema '.
						($this->dbname?
							('AND c.table_catalog=k.table_catalog '):'').
				'LEFT OUTER JOIN '.
					'information_schema.table_constraints AS t ON '.
						'k.table_name=t.table_name AND '.
						'k.constraint_name=t.constraint_name AND '.
						'k.table_schema=t.table_schema '.
						($this->dbname?
							('AND k.table_catalog=t.table_catalog '):'').
				'WHERE '.
					'c.table_name='.$this->quote($table).
					($this->dbname?
						(' AND c.table_catalog='.
							$this->quote($this->dbname)):'').
				';',
				'field','type','defval','nullable','YES','pkey','PRIMARY KEY'),
			'oci'=>array(
				'SELECT c.column_name AS field, '.
					'c.data_type AS type, '.
					'c.data_default AS defval, '.
					'c.nullable AS nullable, '.
					'(SELECT t.constraint_type '.
						'FROM all_cons_columns acc '.
						'LEFT OUTER JOIN all_constraints t '.
						'ON acc.constraint_name=t.constraint_name '.
						'WHERE acc.table_name='.$this->quote($table).' '.
						'AND acc.column_name=c.column_name '.
						'AND constraint_type='.$this->quote('P').') AS pkey '.
				'FROM all_tab_cols c '.
				'WHERE c.table_name='.$this->quote($table),
				'FIELD','TYPE','DEFVAL','NULLABLE','Y','PKEY','P')
		);
		if (is_string($fields))
			$fields=\Base::instance()->split($fields);
		foreach ($cmd as $key=>$val)
			if (preg_match('/'.$key.'/',$this->engine)) {
				// Improve InnoDB performance on MySQL with
				// SET GLOBAL innodb_stats_on_metadata=0;
				// This requires SUPER privilege!
				$rows=array();
				foreach ($this->exec($val[0],NULL,$ttl) as $row) {
					if (!$fields || in_array($row[$val[1]],$fields))
						$rows[$row[$val[1]]]=array(
							'type'=>$row[$val[2]],
							'pdo_type'=>
								preg_match('/int\b|int(?=eger)|bool/i',
									$row[$val[2]],$parts)?
								constant('\PDO::PARAM_'.
									strtoupper($parts[0])):
								\PDO::PARAM_STR,
							'default'=>$row[$val[3]],
							'nullable'=>$row[$val[4]]==$val[5],
							'pkey'=>$row[$val[6]]==$val[7]
						);
				}
				return $rows;
			}
		return FALSE;
	}

	/**
	*	Quote string
	*	@return string
	*	@param $val mixed
	*	@param $type int
	**/
	function quote($val,$type=\PDO::PARAM_STR) {
		return $this->engine=='odbc'?
			(is_string($val)?
				\Base::instance()->stringify(str_replace('\'','\'\'',$val)):
				$val):
			parent::quote($val,$type);
	}

	/**
	*	Return UUID
	*	@return string
	**/
	function uuid() {
		return $this->uuid;
	}

	/**
	*	Return database engine
	*	@return string
	**/
	function driver() {
		return $this->engine;
	}

	/**
	*	Return server version
	*	@return string
	**/
	function version() {
		return parent::getattribute(parent::ATTR_SERVER_VERSION);
	}

	/**
	*	Return database name
	*	@return string
	**/
	function name() {
		return $this->dbname;
	}

	/**
	*	Return quoted identifier name
	*	@return string
	*	@param $key
	**/
	function quotekey($key) {
		if ($this->engine=='mysql')
			$key="`".implode('`.`',explode('.',$key))."`";
		elseif (preg_match('/sybase|dblib/',$this->engine))
			$key="'".implode("'.'",explode('.',$key))."'";
		elseif (preg_match('/sqlite2?|pgsql|oci/',$this->engine))
			$key='"'.implode('"."',explode('.',$key)).'"';
		elseif (preg_match('/mssql|sqlsrv|odbc/',$this->engine))
			$key="[".implode('].[',explode('.',$key))."]";
		return $key;
	}

	/**
	*	Instantiate class
	*	@param $dsn string
	*	@param $user string
	*	@param $pw string
	*	@param $options array
	**/
	function __construct($dsn,$user=NULL,$pw=NULL,array $options=NULL) {
		$fw=\Base::instance();
		$this->uuid=$fw->hash($this->dsn=$dsn);
		if (preg_match('/^.+?(?:dbname|database)=(.+?)(?=;|$)/i',$dsn,$parts))
			$this->dbname=$parts[1];
		if (!$options)
			$options=array();
		if (isset($parts[0]) && strstr($parts[0],':',TRUE)=='mysql')
			$options+=array(\PDO::MYSQL_ATTR_INIT_COMMAND=>'SET NAMES '.
				strtolower(str_replace('-','',$fw->get('ENCODING'))).';');
		parent::__construct($dsn,$user,$pw,$options);
		$this->engine=parent::getattribute(parent::ATTR_DRIVER_NAME);
	}

}
