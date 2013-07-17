<?php

/*
	Copyright (c) 2009-2013 F3::Factory/Bong Cosca, All rights reserved.

	This file is part of the Fat-Free Framework (http://fatfree.sf.net).

	THE SOFTWARE AND DOCUMENTATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF
	ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
	PURPOSE.

	Please see the license.txt file for more information.
*/

namespace DB\SQL;

//! SQL data mapper
class Mapper extends \DB\Cursor {

	//@{ Error messages
	const
		E_Adhoc='Unable to process ad hoc field %s';
	//@}

	protected
		//! PDO wrapper
		$db,
		//! Database engine
		$engine,
		//! SQL table
		$table,
		//! Last insert ID
		$_id,
		//! Defined fields
		$fields,
		//! Adhoc fields
		$adhoc=array();

	/**
	*	Return TRUE if field is defined
	*	@return bool
	*	@param $key string
	**/
	function exists($key) {
		return array_key_exists($key,$this->fields+$this->adhoc);
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
				NULL:$this->value($this->fields[$key]['pdo_type'],$val);
			if ($this->fields[$key]['value']!==$val ||
				$this->fields[$key]['default']!==$val)
				$this->fields[$key]['changed']=TRUE;
			return $this->fields[$key]['value']=$val;
		}
		// Parenthesize expression in case it's a subquery
		$this->adhoc[$key]=array('expr'=>'('.$val.')','value'=>NULL);
		return $val;
	}

	/**
	*	Retrieve value of field
	*	@return scalar
	*	@param $key string
	**/
	function get($key) {
		if ($key=='_id')
			return $this->_id;
		elseif (array_key_exists($key,$this->fields))
			return $this->fields[$key]['value'];
		elseif (array_key_exists($key,$this->adhoc))
			return $this->adhoc[$key]['value'];
		user_error(sprintf(self::E_Field,$key));
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
	*	Convert array to mapper object
	*	@return object
	*	@param $row array
	**/
	protected function factory($row) {
		$mapper=clone($this);
		$mapper->reset();
		foreach ($row as $key=>$val) {
			$var=array_key_exists($key,$this->fields)?'fields':'adhoc';
			$mapper->{$var}[$key]['value']=$val;
			if ($var=='fields' && $mapper->{$var}[$key]['pkey'])
				$mapper->{$var}[$key]['previous']=$val;
		}
		$mapper->query=array(clone($mapper));
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
	*	@return array
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
		if ($options['group'])
			$sql.=' GROUP BY '.$options['group'];
		if ($options['order'])
			$sql.=' ORDER BY '.$options['order'];
		if ($options['limit'])
			$sql.=' LIMIT '.$options['limit'];
		if ($options['offset'])
			$sql.=' OFFSET '.$options['offset'];
		$result=$this->db->exec($sql.';',$args,$ttl);
		$out=array();
		foreach ($result as &$row) {
			foreach ($row as $field=>&$val) {
				if (array_key_exists($field,$this->fields)) {
					if (!is_null($val) || !$this->fields[$field]['nullable'])
						$val=$this->value(
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
	*	@return array
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
		return $this->select('*'.$adhoc,$filter,$options,$ttl);
	}

	/**
	*	Count records that match criteria
	*	@return int
	*	@param $filter string|array
	**/
	function count($filter=NULL) {
		$sql='SELECT COUNT(*) AS rows FROM '.$this->table;
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
		$result=$this->db->exec($sql.';',$args);
		return $result[0]['rows'];
	}

	/**
	*	Return record at specified offset using same criteria as
	*	previous load() call and make it active
	*	@return array
	*	@param $ofs int
	**/
	function skip($ofs=1) {
		if ($out=parent::skip($ofs)) {
			foreach ($this->fields as $key=>&$field) {
				$field['value']=$out->fields[$key]['value'];
				$field['changed']=FALSE;
				if ($field['pkey'])
					$field['previous']=$out->fields[$key]['value'];
				unset($field);
			}
			foreach ($this->adhoc as $key=>&$field) {
				$field['value']=$out->adhoc[$key]['value'];
				unset($field);
			}
		}
		return $out;
	}

	/**
	*	Insert new record
	*	@return array
	**/
	function insert() {
		$args=array();
		$ctr=0;
		$fields='';
		$values='';
		$pkeys=array();
		$inc=NULL;
		foreach ($this->fields as $key=>&$field) {
			if ($field['pkey']) {
				$pkeys[]=$key;
				$field['previous']=$field['value'];
				if (!$inc && $field['pdo_type']==\PDO::PARAM_INT &&
					empty($field['value']) && !$field['nullable'])
					$inc=$key;
			}
			if ($field['changed'] && $key!=$inc) {
				$fields.=($ctr?',':'').$this->db->quotekey($key);
				$values.=($ctr?',':'').'?';
				$args[$ctr+1]=array($field['value'],$field['pdo_type']);
				$ctr++;
			}
			$field['changed']=FALSE;
			unset($field);
		}
		if ($fields)
			$this->db->exec(
				'INSERT INTO '.$this->table.' ('.$fields.') '.
				'VALUES ('.$values.');',$args
			);
		$seq=NULL;
		if ($this->engine=='pgsql')
			$seq=$this->table.'_'.end($pkeys).'_seq';
		$this->_id=$this->db->lastinsertid($seq);
		if (!$inc) {
			$ctr=0;
			$query='';
			$args='';
			foreach ($pkeys as $pkey) {
				$query.=($query?' AND ':'').$this->db->quotekey($pkey).'=?';
				$args[$ctr+1]=$this->fields[$pkey]['value'];
				$ctr++;
			}
			return $this->load(array($query,$args));
		}
		// Reload to obtain default and auto-increment field values
		return $this->load(array($inc.'=?',
			$this->value($this->fields[$inc]['pdo_type'],$this->_id)));
	}

	/**
	*	Update current record
	*	@return array
	**/
	function update() {
		$args=array();
		$ctr=0;
		$pairs='';
		$filter='';
		foreach ($this->fields as $key=>$field)
			if ($field['changed']) {
				$pairs.=($pairs?',':'').$this->db->quotekey($key).'=?';
				$args[$ctr+1]=array($field['value'],$field['pdo_type']);
				$ctr++;
			}
		foreach ($this->fields as $key=>$field)
			if ($field['pkey']) {
				$filter.=($filter?' AND ':'').$this->db->quotekey($key).'=?';
				$args[$ctr+1]=array($field['previous'],$field['pdo_type']);
				$ctr++;
			}
		if ($pairs) {
			$sql='UPDATE '.$this->table.' SET '.$pairs;
			if ($filter)
				$sql.=' WHERE '.$filter;
			return $this->db->exec($sql.';',$args);
		}
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
		foreach ($this->fields as $key=>&$field) {
			if ($field['pkey']) {
				$filter.=($filter?' AND ':'').$this->db->quotekey($key).'=?';
				$args[$ctr+1]=array($field['previous'],$field['pdo_type']);
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
		$this->skip(0);
		return $this->db->
			exec('DELETE FROM '.$this->table.' WHERE '.$filter.';',$args);
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
	*	@param $key string
	**/
	function copyfrom($key) {
		foreach (\Base::instance()->get($key) as $key=>$val)
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
		foreach ($this->fields as $key=>$field)
			$var[$key]=$field['value'];
	}

	/**
	*	Return schema
	*	@return array
	**/
	function schema() {
		return $this->fields;
	}

	/**
	*	Instantiate class
	*	@param $db object
	*	@param $table string
	*	@param $ttl int
	**/
	function __construct(\DB\SQL $db,$table,$ttl=60) {
		$this->db=$db;
		$this->engine=$db->driver();
		$this->table=$this->db->quotekey($table);
		$this->fields=$db->schema($table,$ttl);
		$this->reset();
	}

}
