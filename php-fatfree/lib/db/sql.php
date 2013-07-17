<?php

namespace DB;

/*
	Copyright (c) 2009-2013 F3::Factory/Bong Cosca, All rights reserved.

	This file is part of the Fat-Free Framework (http://fatfree.sf.net).

	THE SOFTWARE AND DOCUMENTATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF
	ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
	PURPOSE.

	Please see the license.txt file for more information.
*/

//! PDO wrapper
class SQL extends \PDO {

	private
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
	*	@return NULL
	**/
	function begin() {
		parent::begintransaction();
		$this->trans=TRUE;
	}

	/**
	*	Rollback SQL transaction
	*	@return NULL
	**/
	function rollback() {
		parent::rollback();
		$this->trans=FALSE;
	}

	/**
	*	Commit SQL transaction
	*	@return NULL
	**/
	function commit() {
		parent::commit();
		$this->trans=FALSE;
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
		foreach (array_combine($cmds,$args) as $cmd=>$arg) {
			$now=microtime(TRUE);
			$keys=$vals=array();
			if ($fw->get('CACHE') && $ttl && ($cached=$cache->exists(
				$hash=$fw->hash($cmd.$fw->stringify($arg)).'.sql',
				$result)) && $cached[0]+$ttl>microtime(TRUE)) {
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
						$vals[]=$fw->stringify($val[0]);
					}
					else {
						// Convert to PDO data type
						$query->bindvalue($key,$val,$this->type($val));
						$vals[]=$fw->stringify($val);
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
				if (preg_match(
					'/\b(?:CALL|EXPLAIN|SELECT|PRAGMA|SHOW)\b/i',$cmd)) {
					$result=$query->fetchall(\PDO::FETCH_ASSOC);
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
	*	@param $ttl int
	**/
	function schema($table,$ttl=0) {
		// Supported engines
		$cmd=array(
			'sqlite2?'=>array(
				'PRAGMA table_info("'.$table.'");',
				'name','type','dflt_value','notnull',0,'pk',1),
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
						'c.column_name=k.column_name '.
						($this->dbname?
							('AND '.
							($this->engine=='pgsql'?
								'c.table_catalog=k.table_catalog':
								'c.table_schema=k.table_schema').' '):'').
				'LEFT OUTER JOIN '.
					'information_schema.table_constraints AS t ON '.
						'k.table_name=t.table_name AND '.
						'k.constraint_name=t.constraint_name '.
						($this->dbname?
							('AND '.
							($this->engine=='pgsql'?
								'k.table_catalog=t.table_catalog':
								'k.table_schema=t.table_schema').' '):'').
				'WHERE '.
					'c.table_name='.$this->quote($table).' '.
					($this->dbname?
						('AND '.
							($this->engine=='pgsql'?
							'c.table_catalog':'c.table_schema').
							'='.$this->quote($this->dbname)):'').
				';',
				'field','type','defval','nullable','YES','pkey','PRIMARY KEY')
		);
		foreach ($cmd as $key=>$val)
			if (preg_match('/'.$key.'/',$this->engine)) {
				$rows=array();
				foreach ($this->exec($val[0],NULL,$ttl) as $row)
					$rows[$row[$val[1]]]=array(
						'type'=>$row[$val[2]],
						'pdo_type'=>
							preg_match('/int|bool/i',$row[$val[2]],$parts)?
							constant('\PDO::PARAM_'.strtoupper($parts[0])):
							\PDO::PARAM_STR,
						'default'=>$row[$val[3]],
						'nullable'=>$row[$val[4]]==$val[5],
						'pkey'=>$row[$val[6]]==$val[7]
					);
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
			$key="`".$key."`";
		elseif (preg_match('/sybase|dblib/',$this->engine))
			$key="'".$key."'";
		elseif (preg_match('/sqlite2?|pgsql/',$this->engine))
			$key='"'.$key.'"';
		elseif (preg_match('/mssql|sqlsrv|odbc/',$this->engine))
			$key="[".$key."]";
		elseif ($this->engine=='oci')
			$key='"'.strtoupper($key).'"';
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
		if (preg_match('/^.+?(?:dbname|database)=(.+?)(?=;|$)/i',$dsn,$parts))
			$this->dbname=$parts[1];
		if (!$options)
			$options=array();
		$options+=array(\PDO::ATTR_EMULATE_PREPARES=>FALSE);
		if (isset($parts[0]) && strstr($parts[0],':',TRUE)=='mysql')
			$options+=array(\PDO::MYSQL_ATTR_INIT_COMMAND=>'SET NAMES '.
				strtolower(str_replace('-','',
					\Base::instance()->get('ENCODING'))).';');
		parent::__construct($dsn,$user,$pw,$options);
		$this->engine=parent::getattribute(parent::ATTR_DRIVER_NAME);
	}

}
