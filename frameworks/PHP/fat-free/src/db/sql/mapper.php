<?php

/*

	Copyright (c) 2009-2015 F3::Factory/Bong Cosca, All rights reserved.

	This file is part of the Fat-Free Framework (http://fatfreeframework.com).

	This is free software: you can redistribute it and/or modify it under the
	terms of the GNU General Public License as published by the Free Software
	Foundation, either version 3 of the License, or later.

	Fat-Free Framework is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	General Public License for more details.

	You should have received a copy of the GNU General Public License along
	with Fat-Free Framework.  If not, see <http://www.gnu.org/licenses/>.

*/

namespace DB\SQL;

//! SQL data mapper
class Mapper extends \DB\Cursor {

	protected
		//! PDO wrapper
		$db,
		//! Database engine
		$engine,
		//! SQL table
		$source,
		//! SQL table (quoted)
		$table,
		//! Last insert ID
		$_id,
		//! Defined fields
		$fields,
		//! Adhoc fields
		$adhoc=array();

	/**
	*	Return database type
	*	@return string
	**/
	function dbtype() {
		return 'SQL';
	}

	/**
	*	Return mapped table
	*	@return string
	**/
	function table() {
		return $this->source;
	}

	/**
	*	Return TRUE if field is defined
	*	@return bool
	*	@param $key string
	**/
	function exists($key) {
		return array_key_exists($key,$this->fields+$this->adhoc);
	}

	/**
	*	Return TRUE if any/specified field value has changed
	*	@return bool
	*	@param $key string
	**/
	function changed($key=NULL) {
		if (isset($key))
			return $this->fields[$key]['changed'];
		foreach($this->fields as $key=>$field)
			if ($field['changed'])
				return TRUE;
		return FALSE;
	}

	/**
	*	Assign value to field
	*	@return scalar
	*	@param $key string
	*	@param $val scalar
	**/
	function set($key,$val) {
		if (array_key_exists($key,$this->fields)) {
			$val=is_null($val) && $this->fields[$key]['nullable']?
				NULL:$this->db->value($this->fields[$key]['pdo_type'],$val);
			if ($this->fields[$key]['value']!==$val ||
				$this->fields[$key]['default']!==$val && is_null($val))
				$this->fields[$key]['changed']=TRUE;
			return $this->fields[$key]['value']=$val;
		}
		// adjust result on existing expressions
		if (isset($this->adhoc[$key]))
			$this->adhoc[$key]['value']=$val;
		else
			// Parenthesize expression in case it's a subquery
			$this->adhoc[$key]=array('expr'=>'('.$val.')','value'=>NULL);
		return $val;
	}

	/**
	*	Retrieve value of field
	*	@return scalar
	*	@param $key string
	**/
	function &get($key) {
		if ($key=='_id')
			return $this->_id;
		elseif (array_key_exists($key,$this->fields))
			return $this->fields[$key]['value'];
		elseif (array_key_exists($key,$this->adhoc))
			return $this->adhoc[$key]['value'];
		user_error(sprintf(self::E_Field,$key),E_USER_ERROR);
	}

	/**
	*	Clear value of field
	*	@return NULL
	*	@param $key string
	**/
	function clear($key) {
		if (array_key_exists($key,$this->adhoc))
			unset($this->adhoc[$key]);
	}

	/**
	*	Get PHP type equivalent of PDO constant
	*	@return string
	*	@param $pdo string
	**/
	function type($pdo) {
		switch ($pdo) {
			case \PDO::PARAM_NULL:
				return 'unset';
			case \PDO::PARAM_INT:
				return 'int';
			case \PDO::PARAM_BOOL:
				return 'bool';
			case \PDO::PARAM_STR:
				return 'string';
		}
	}

	/**
	*	Convert array to mapper object
	*	@return object
	*	@param $row array
	**/
	protected function factory($row) {
		$mapper=clone($this);
		$mapper->reset();
		foreach ($row as $key=>$val) {
			if (array_key_exists($key,$this->fields))
				$var='fields';
			elseif (array_key_exists($key,$this->adhoc))
				$var='adhoc';
			else
				continue;
			$mapper->{$var}[$key]['value']=$val;
			if ($var=='fields' && $mapper->{$var}[$key]['pkey'])
				$mapper->{$var}[$key]['previous']=$val;
		}
		$mapper->query=array(clone($mapper));
		if (isset($mapper->trigger['load']))
			\Base::instance()->call($mapper->trigger['load'],$mapper);
		return $mapper;
	}

	/**
	*	Return fields of mapper object as an associative array
	*	@return array
	*	@param $obj object
	**/
	function cast($obj=NULL) {
		if (!$obj)
			$obj=$this;
		return array_map(
			function($row) {
				return $row['value'];
			},
			$obj->fields+$obj->adhoc
		);
	}

	/**
	*	Build query string and execute
	*	@return \DB\SQL\Mapper[]
	*	@param $fields string
	*	@param $filter string|array
	*	@param $options array
	*	@param $ttl int
	**/
	function select($fields,$filter=NULL,array $options=NULL,$ttl=0) {
		if (!$options)
			$options=array();
		$options+=array(
			'group'=>NULL,
			'order'=>NULL,
			'limit'=>0,
			'offset'=>0
		);
		$db=$this->db;
		$sql='SELECT '.$fields.' FROM '.$this->table;
		$args=array();
		if ($filter) {
			if (is_array($filter)) {
				$args=isset($filter[1]) && is_array($filter[1])?
					$filter[1]:
					array_slice($filter,1,NULL,TRUE);
				$args=is_array($args)?$args:array(1=>$args);
				list($filter)=$filter;
			}
			$sql.=' WHERE '.$filter;
		}
		if ($options['group']) {
			$sql.=' GROUP BY '.implode(',',array_map(
				function($str) use($db) {
					return preg_replace_callback(
						'/\b(\w+)\h*(HAVING.+|$)/i',
						function($parts) use($db) {
							return $db->quotekey($parts[1]);
						},
						$str
					);
				},
				explode(',',$options['group'])));
		}
		if ($options['order']) {
			$sql.=' ORDER BY '.implode(',',array_map(
				function($str) use($db) {
					return preg_match('/^(\w+)(?:\h+(ASC|DESC))?\h*(?:,|$)/i',
						$str,$parts)?
						($db->quotekey($parts[1]).
						(isset($parts[2])?(' '.$parts[2]):'')):$str;
				},
				explode(',',$options['order'])));
		}
		if (preg_match('/mssql|sqlsrv|odbc/', $this->engine) &&
			($options['limit'] || $options['offset'])) {
			$pkeys=array();
			foreach ($this->fields as $key=>$field)
				if ($field['pkey'])
					$pkeys[]=$key;
			$ofs=$options['offset']?(int)$options['offset']:0;
			$lmt=$options['limit']?(int)$options['limit']:0;
			if (strncmp($db->version(),'11',2)>=0) {
				// SQL Server 2012
				if (!$options['order'])
					$sql.=' ORDER BY '.$db->quotekey($pkeys[0]);
				$sql.=' OFFSET '.$ofs.' ROWS';
				if ($lmt)
					$sql.=' FETCH NEXT '.$lmt.' ROWS ONLY';
			}
			else {
				// SQL Server 2008
				$sql=str_replace('SELECT',
					'SELECT '.
					($lmt>0?'TOP '.($ofs+$lmt):'').' ROW_NUMBER() '.
					'OVER (ORDER BY '.
						$db->quotekey($pkeys[0]).') AS rnum,',$sql);
				$sql='SELECT * FROM ('.$sql.') x WHERE rnum > '.($ofs);
			}
		}
		else {
			if ($options['limit'])
				$sql.=' LIMIT '.(int)$options['limit'];
			if ($options['offset'])
				$sql.=' OFFSET '.(int)$options['offset'];
		}
		$result=$this->db->exec($sql,$args,$ttl);
		$out=array();
		foreach ($result as &$row) {
			foreach ($row as $field=>&$val) {
				if (array_key_exists($field,$this->fields)) {
					if (!is_null($val) || !$this->fields[$field]['nullable'])
						$val=$this->db->value(
							$this->fields[$field]['pdo_type'],$val);
				}
				elseif (array_key_exists($field,$this->adhoc))
					$this->adhoc[$field]['value']=$val;
				unset($val);
			}
			$out[]=$this->factory($row);
			unset($row);
		}
		return $out;
	}

	/**
	*	Return records that match criteria
	*	@return \DB\SQL\Mapper[]
	*	@param $filter string|array
	*	@param $options array
	*	@param $ttl int
	**/
	function find($filter=NULL,array $options=NULL,$ttl=0) {
		if (!$options)
			$options=array();
		$options+=array(
			'group'=>NULL,
			'order'=>NULL,
			'limit'=>0,
			'offset'=>0
		);
		$adhoc='';
		foreach ($this->adhoc as $key=>$field)
			$adhoc.=','.$field['expr'].' AS '.$this->db->quotekey($key);
		return $this->select(
			($options['group'] && !preg_match('/mysql|sqlite/',$this->engine)?
				$options['group']:
				implode(',',array_map(array($this->db,'quotekey'),
					array_keys($this->fields)))).$adhoc,$filter,$options,$ttl);
	}

	/**
	*	Count records that match criteria
	*	@return int
	*	@param $filter string|array
	*	@param $ttl int
	**/
	function count($filter=NULL,$ttl=0) {
		$sql='SELECT COUNT(*) AS '.
			$this->db->quotekey('rows').' FROM '.$this->table;
		$args=array();
		if ($filter) {
			if (is_array($filter)) {
				$args=isset($filter[1]) && is_array($filter[1])?
					$filter[1]:
					array_slice($filter,1,NULL,TRUE);
				$args=is_array($args)?$args:array(1=>$args);
				list($filter)=$filter;
			}
			$sql.=' WHERE '.$filter;
		}
		$result=$this->db->exec($sql,$args,$ttl);
		return $result[0]['rows'];
	}

	/**
	*	Return record at specified offset using same criteria as
	*	previous load() call and make it active
	*	@return array
	*	@param $ofs int
	**/
	function skip($ofs=1) {
		$out=parent::skip($ofs);
		$dry=$this->dry();
		foreach ($this->fields as $key=>&$field) {
			$field['value']=$dry?NULL:$out->fields[$key]['value'];
			$field['changed']=FALSE;
			if ($field['pkey'])
				$field['previous']=$dry?NULL:$out->fields[$key]['value'];
			unset($field);
		}
		foreach ($this->adhoc as $key=>&$field) {
			$field['value']=$dry?NULL:$out->adhoc[$key]['value'];
			unset($field);
		}
		if (isset($this->trigger['load']))
			\Base::instance()->call($this->trigger['load'],$this);
		return $out;
	}

	/**
	*	Insert new record
	*	@return object
	**/
	function insert() {
		$args=array();
		$actr=0;
		$nctr=0;
		$fields='';
		$values='';
		$filter='';
		$pkeys=array();
		$nkeys=array();
		$ckeys=array();
		$inc=NULL;
		foreach ($this->fields as $key=>$field)
			if ($field['pkey'])
				$pkeys[$key]=$field['previous'];
		if (isset($this->trigger['beforeinsert']) &&
			\Base::instance()->call($this->trigger['beforeinsert'],
				array($this,$pkeys))===FALSE)
			return $this;
		foreach ($this->fields as $key=>&$field) {
			if ($field['pkey']) {
				$field['previous']=$field['value'];
				if (!$inc && $field['pdo_type']==\PDO::PARAM_INT &&
					empty($field['value']) && !$field['nullable'])
					$inc=$key;
				$filter.=($filter?' AND ':'').$this->db->quotekey($key).'=?';
				$nkeys[$nctr+1]=array($field['value'],$field['pdo_type']);
				$nctr++;
			}
			if ($field['changed'] && $key!=$inc) {
				$fields.=($actr?',':'').$this->db->quotekey($key);
				$values.=($actr?',':'').'?';
				$args[$actr+1]=array($field['value'],$field['pdo_type']);
				$actr++;
				$ckeys[]=$key;
			}
			$field['changed']=FALSE;
			unset($field);
		}
		if ($fields) {
			$this->db->exec(
				(preg_match('/mssql|dblib|sqlsrv/',$this->engine) &&
				array_intersect(array_keys($pkeys),$ckeys)?
					'SET IDENTITY_INSERT '.$this->table.' ON;':'').
				'INSERT INTO '.$this->table.' ('.$fields.') '.
				'VALUES ('.$values.')',$args
			);
			$seq=NULL;
			if ($this->engine=='pgsql') {
				$names=array_keys($pkeys);
				$seq=$this->source.'_'.end($names).'_seq';
			}
			if ($this->engine!='oci')
				$this->_id=$this->db->lastinsertid($seq);
			// Reload to obtain default and auto-increment field values
			$this->load($inc?
				array($inc.'=?',$this->db->value(
					$this->fields[$inc]['pdo_type'],$this->_id)):
				array($filter,$nkeys));
			if (isset($this->trigger['afterinsert']))
				\Base::instance()->call($this->trigger['afterinsert'],
					array($this,$pkeys));
		}
		return $this;
	}

	/**
	*	Update current record
	*	@return object
	**/
	function update() {
		$args=array();
		$ctr=0;
		$pairs='';
		$filter='';
		$pkeys=array();
		foreach ($this->fields as $key=>$field)
			if ($field['pkey'])
				$pkeys[$key]=$field['previous'];
		if (isset($this->trigger['beforeupdate']) &&
			\Base::instance()->call($this->trigger['beforeupdate'],
				array($this,$pkeys))===FALSE)
			return $this;
		foreach ($this->fields as $key=>$field)
			if ($field['changed']) {
				$pairs.=($pairs?',':'').$this->db->quotekey($key).'=?';
				$args[$ctr+1]=array($field['value'],$field['pdo_type']);
				$ctr++;
			}
		foreach ($this->fields as $key=>$field)
			if ($field['pkey']) {
				$filter.=($filter?' AND ':' WHERE ').
					$this->db->quotekey($key).'=?';
				$args[$ctr+1]=array($field['previous'],$field['pdo_type']);
				$ctr++;
			}
		if ($pairs) {
			$sql='UPDATE '.$this->table.' SET '.$pairs.$filter;
			$this->db->exec($sql,$args);
			if (isset($this->trigger['afterupdate']))
				\Base::instance()->call($this->trigger['afterupdate'],
					array($this,$pkeys));
		}
		return $this;
	}

	/**
	*	Delete current record
	*	@return int
	*	@param $filter string|array
	**/
	function erase($filter=NULL) {
		if ($filter) {
			$args=array();
			if (is_array($filter)) {
				$args=isset($filter[1]) && is_array($filter[1])?
					$filter[1]:
					array_slice($filter,1,NULL,TRUE);
				$args=is_array($args)?$args:array(1=>$args);
				list($filter)=$filter;
			}
			return $this->db->
				exec('DELETE FROM '.$this->table.' WHERE '.$filter.';',$args);
		}
		$args=array();
		$ctr=0;
		$filter='';
		$pkeys=array();
		foreach ($this->fields as $key=>&$field) {
			if ($field['pkey']) {
				$filter.=($filter?' AND ':'').$this->db->quotekey($key).'=?';
				$args[$ctr+1]=array($field['previous'],$field['pdo_type']);
				$pkeys[$key]=$field['previous'];
				$ctr++;
			}
			$field['value']=NULL;
			$field['changed']=(bool)$field['default'];
			if ($field['pkey'])
				$field['previous']=NULL;
			unset($field);
		}
		foreach ($this->adhoc as &$field) {
			$field['value']=NULL;
			unset($field);
		}
		parent::erase();
		if (isset($this->trigger['beforeerase']) &&
			\Base::instance()->call($this->trigger['beforeerase'],
				array($this,$pkeys))===FALSE)
			return 0;
		$out=$this->db->
			exec('DELETE FROM '.$this->table.' WHERE '.$filter.';',$args);
		if (isset($this->trigger['aftererase']))
			\Base::instance()->call($this->trigger['aftererase'],
				array($this,$pkeys));
		return $out;
	}

	/**
	*	Reset cursor
	*	@return NULL
	**/
	function reset() {
		foreach ($this->fields as &$field) {
			$field['value']=NULL;
			$field['changed']=FALSE;
			if ($field['pkey'])
				$field['previous']=NULL;
			unset($field);
		}
		foreach ($this->adhoc as &$field) {
			$field['value']=NULL;
			unset($field);
		}
		parent::reset();
	}

	/**
	*	Hydrate mapper object using hive array variable
	*	@return NULL
	*	@param $var array|string
	*	@param $func callback
	**/
	function copyfrom($var,$func=NULL) {
		if (is_string($var))
			$var=\Base::instance()->get($var);
		if ($func)
			$var=call_user_func($func,$var);
		foreach ($var as $key=>$val)
			if (in_array($key,array_keys($this->fields))) {
				$field=&$this->fields[$key];
				if ($field['value']!==$val) {
					$field['value']=$val;
					$field['changed']=TRUE;
				}
				unset($field);
			}
	}

	/**
	*	Populate hive array variable with mapper fields
	*	@return NULL
	*	@param $key string
	**/
	function copyto($key) {
		$var=&\Base::instance()->ref($key);
		foreach ($this->fields+$this->adhoc as $key=>$field)
			$var[$key]=$field['value'];
	}

	/**
	*	Return schema and, if the first argument is provided, update it
	*	@return array
	*	@param $fields NULL|array
	**/
	function schema($fields=null) {
		if ($fields)
			$this->fields = $fields;
		return $this->fields;
	}

	/**
	*	Return field names
	*	@return array
	*	@param $adhoc bool
	**/
	function fields($adhoc=TRUE) {
		return array_keys($this->fields+($adhoc?$this->adhoc:array()));
	}

	/**
	*	Return TRUE if field is not nullable
	*	@return bool
	*	@param $field string
	**/
	function required($field) {
		return isset($this->fields[$field]) &&
			!$this->fields[$field]['nullable'];
	}

	/**
	*	Retrieve external iterator for fields
	*	@return object
	**/
	function getiterator() {
		return new \ArrayIterator($this->cast());
	}

	/**
	*	Instantiate class
	*	@param $db object
	*	@param $table string
	*	@param $fields array|string
	*	@param $ttl int
	**/
	function __construct(\DB\SQL $db,$table,$fields=NULL,$ttl=60) {
		$this->db=$db;
		$this->engine=$db->driver();
		if ($this->engine=='oci')
			$table=strtoupper($table);
		$this->source=$table;
		$this->table=$this->db->quotekey($table);
		$this->fields=$db->schema($table,$fields,$ttl);
		$this->reset();
	}

}
