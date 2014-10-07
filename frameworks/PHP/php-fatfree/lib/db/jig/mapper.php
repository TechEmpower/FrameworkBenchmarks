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

namespace DB\Jig;

//! Flat-file DB mapper
class Mapper extends \DB\Cursor {

	protected
		//! Flat-file DB wrapper
		$db,
		//! Data file
		$file,
		//! Document identifier
		$id,
		//! Document contents
		$document=array();

	/**
	*	Return database type
	*	@return string
	**/
	function dbtype() {
		return 'Jig';
	}

	/**
	*	Return TRUE if field is defined
	*	@return bool
	*	@param $key string
	**/
	function exists($key) {
		return array_key_exists($key,$this->document);
	}

	/**
	*	Assign value to field
	*	@return scalar|FALSE
	*	@param $key string
	*	@param $val scalar
	**/
	function set($key,$val) {
		return ($key=='_id')?FALSE:($this->document[$key]=$val);
	}

	/**
	*	Retrieve value of field
	*	@return scalar|FALSE
	*	@param $key string
	**/
	function get($key) {
		if ($key=='_id')
			return $this->id;
		if (array_key_exists($key,$this->document))
			return $this->document[$key];
		user_error(sprintf(self::E_Field,$key));
		return FALSE;
	}

	/**
	*	Delete field
	*	@return NULL
	*	@param $key string
	**/
	function clear($key) {
		if ($key!='_id')
			unset($this->document[$key]);
	}

	/**
	*	Convert array to mapper object
	*	@return object
	*	@param $id string
	*	@param $row array
	**/
	protected function factory($id,$row) {
		$mapper=clone($this);
		$mapper->reset();
		$mapper->id=$id;
		foreach ($row as $field=>$val)
			$mapper->document[$field]=$val;
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
		return $obj->document+array('_id'=>$this->id);
	}

	/**
	*	Convert tokens in string expression to variable names
	*	@return string
	*	@param $str string
	**/
	function token($str) {
		$self=$this;
		$str=preg_replace_callback(
			'/(?<!\w)@(\w(?:[\w\.\[\]])*)/',
			function($token) use($self) {
				// Convert from JS dot notation to PHP array notation
				return '$'.preg_replace_callback(
					'/(\.\w+)|\[((?:[^\[\]]*|(?R))*)\]/',
					function($expr) use($self) {
						$fw=\Base::instance();
						return
							'['.
							($expr[1]?
								$fw->stringify(substr($expr[1],1)):
								(preg_match('/^\w+/',
									$mix=$self->token($expr[2]))?
									$fw->stringify($mix):
									$mix)).
							']';
					},
					$token[1]
				);
			},
			$str
		);
		return trim($str);
	}

	/**
	*	Return records that match criteria
	*	@return array|FALSE
	*	@param $filter array
	*	@param $options array
	*	@param $ttl int
	*	@param $log bool
	**/
	function find($filter=NULL,array $options=NULL,$ttl=0,$log=TRUE) {
		if (!$options)
			$options=array();
		$options+=array(
			'order'=>NULL,
			'limit'=>0,
			'offset'=>0
		);
		$fw=\Base::instance();
		$cache=\Cache::instance();
		$db=$this->db;
		$now=microtime(TRUE);
		$data=array();
		if (!$fw->get('CACHE') || !$ttl || !($cached=$cache->exists(
			$hash=$fw->hash($this->db->dir().
				$fw->stringify(array($filter,$options))).'.jig',$data)) ||
			$cached[0]+$ttl<microtime(TRUE)) {
			$data=$db->read($this->file);
			if (is_null($data))
				return FALSE;
			foreach ($data as $id=>&$doc) {
				$doc['_id']=$id;
				unset($doc);
			}
			if ($filter) {
				if (!is_array($filter))
					return FALSE;
				// Normalize equality operator
				$expr=preg_replace('/(?<=[^<>!=])=(?!=)/','==',$filter[0]);
				// Prepare query arguments
				$args=isset($filter[1]) && is_array($filter[1])?
					$filter[1]:
					array_slice($filter,1,NULL,TRUE);
				$args=is_array($args)?$args:array(1=>$args);
				$keys=$vals=array();
				$tokens=array_slice(
					token_get_all('<?php '.$this->token($expr)),1);
				$data=array_filter($data,
					function($_row) use($fw,$args,$tokens) {
						$_expr='';
						$ctr=0;
						$named=FALSE;
						foreach ($tokens as $token) {
							if (is_string($token))
								if ($token=='?') {
									// Positional
									$ctr++;
									$key=$ctr;
								}
								else {
									if ($token==':')
										$named=TRUE;
									else
										$_expr.=$token;
									continue;
								}
							elseif ($named &&
								token_name($token[0])=='T_STRING') {
								$key=':'.$token[1];
								$named=FALSE;
							}
							else {
								$_expr.=$token[1];
								continue;
							}
							$_expr.=$fw->stringify(
								is_string($args[$key])?
									addcslashes($args[$key],'\''):
									$args[$key]);
						}
						// Avoid conflict with user code
						unset($fw,$tokens,$args,$ctr,$token,$key,$named);
						extract($_row);
						// Evaluate pseudo-SQL expression
						return eval('return '.$_expr.';');
					}
				);
			}
			if (isset($options['order'])) {
				$cols=$fw->split($options['order']);
				uasort(
					$data,
					function($val1,$val2) use($cols) {
						foreach ($cols as $col) {
							$parts=explode(' ',$col,2);
							$order=empty($parts[1])?
								SORT_ASC:
								constant($parts[1]);
							$col=$parts[0];
							if (!array_key_exists($col,$val1))
								$val1[$col]=NULL;
							if (!array_key_exists($col,$val2))
								$val2[$col]=NULL;
							list($v1,$v2)=array($val1[$col],$val2[$col]);
							if ($out=strnatcmp($v1,$v2)*
								(($order==SORT_ASC)*2-1))
								return $out;
						}
						return 0;
					}
				);
			}
			$data=array_slice($data,
				$options['offset'],$options['limit']?:NULL,TRUE);
			if ($fw->get('CACHE') && $ttl)
				// Save to cache backend
				$cache->set($hash,$data,$ttl);
		}
		$out=array();
		foreach ($data as $id=>&$doc) {
			unset($doc['_id']);
			$out[]=$this->factory($id,$doc);
			unset($doc);
		}
		if ($log && isset($args)) {
			if ($filter)
				foreach ($args as $key=>$val) {
					$vals[]=$fw->stringify(is_array($val)?$val[0]:$val);
					$keys[]='/'.(is_numeric($key)?'\?':preg_quote($key)).'/';
				}
			$db->jot('('.sprintf('%.1f',1e3*(microtime(TRUE)-$now)).'ms) '.
				$this->file.' [find] '.
				($filter?preg_replace($keys,$vals,$filter[0],1):''));
		}
		return $out;
	}

	/**
	*	Count records that match criteria
	*	@return int
	*	@param $filter array
	*	@param $ttl int
	**/
	function count($filter=NULL,$ttl=0) {
		$now=microtime(TRUE);
		$out=count($this->find($filter,NULL,$ttl,FALSE));
		$this->db->jot('('.sprintf('%.1f',1e3*(microtime(TRUE)-$now)).'ms) '.
			$this->file.' [count] '.($filter?json_encode($filter):''));
		return $out;
	}

	/**
	*	Return record at specified offset using criteria of previous
	*	load() call and make it active
	*	@return array
	*	@param $ofs int
	**/
	function skip($ofs=1) {
		$this->document=($out=parent::skip($ofs))?$out->document:array();
		$this->id=$out?$out->id:NULL;
		if ($this->document && isset($this->trigger['load']))
			\Base::instance()->call($this->trigger['load'],$this);
		return $out;
	}

	/**
	*	Insert new record
	*	@return array
	**/
	function insert() {
		if ($this->id)
			return $this->update();
		$db=$this->db;
		$now=microtime(TRUE);
		while (($id=uniqid(NULL,TRUE)) &&
			($data=$db->read($this->file)) && isset($data[$id]) &&
			!connection_aborted())
			usleep(mt_rand(0,100));
		$this->id=$id;
		$data[$id]=$this->document;
		$pkey=array('_id'=>$this->id);
		if (isset($this->trigger['beforeinsert']))
			\Base::instance()->call($this->trigger['beforeinsert'],
				array($this,$pkey));
		$db->write($this->file,$data);
		$db->jot('('.sprintf('%.1f',1e3*(microtime(TRUE)-$now)).'ms) '.
			$this->file.' [insert] '.json_encode($this->document));
		if (isset($this->trigger['afterinsert']))
			\Base::instance()->call($this->trigger['afterinsert'],
				array($this,$pkey));
		$this->load(array('@_id=?',$this->id));
		return $this->document;
	}

	/**
	*	Update current record
	*	@return array
	**/
	function update() {
		$db=$this->db;
		$now=microtime(TRUE);
		$data=$db->read($this->file);
		$data[$this->id]=$this->document;
		if (isset($this->trigger['beforeupdate']))
			\Base::instance()->call($this->trigger['beforeupdate'],
				array($this,array('_id'=>$this->id)));
		$db->write($this->file,$data);
		$db->jot('('.sprintf('%.1f',1e3*(microtime(TRUE)-$now)).'ms) '.
			$this->file.' [update] '.json_encode($this->document));
		if (isset($this->trigger['afterupdate']))
			\Base::instance()->call($this->trigger['afterupdate'],
				array($this,array('_id'=>$this->id)));
		return $this->document;
	}

	/**
	*	Delete current record
	*	@return bool
	*	@param $filter array
	**/
	function erase($filter=NULL) {
		$db=$this->db;
		$now=microtime(TRUE);
		$data=$db->read($this->file);
		$pkey=array('_id'=>$this->id);
		if ($filter) {
			foreach ($this->find($filter,NULL,FALSE) as $mapper)
				if (!$mapper->erase())
					return FALSE;
			return TRUE;
		}
		elseif (isset($this->id)) {
			unset($data[$this->id]);
			parent::erase();
			$this->skip(0);
		}
		else
			return FALSE;
		if (isset($this->trigger['beforeerase']))
			\Base::instance()->call($this->trigger['beforeerase'],
				array($this,$pkey));
		$db->write($this->file,$data);
		if ($filter) {
			$args=isset($filter[1]) && is_array($filter[1])?
				$filter[1]:
				array_slice($filter,1,NULL,TRUE);
			$args=is_array($args)?$args:array(1=>$args);
			foreach ($args as $key=>$val) {
				$vals[]=\Base::instance()->
					stringify(is_array($val)?$val[0]:$val);
				$keys[]='/'.(is_numeric($key)?'\?':preg_quote($key)).'/';
			}
		}
		$db->jot('('.sprintf('%.1f',1e3*(microtime(TRUE)-$now)).'ms) '.
			$this->file.' [erase] '.
			($filter?preg_replace($keys,$vals,$filter[0],1):''));
		if (isset($this->trigger['aftererase']))
			\Base::instance()->call($this->trigger['aftererase'],
				array($this,$pkey));
		return TRUE;
	}

	/**
	*	Reset cursor
	*	@return NULL
	**/
	function reset() {
		$this->id=NULL;
		$this->document=array();
		parent::reset();
	}

	/**
	*	Hydrate mapper object using hive array variable
	*	@return NULL
	*	@param $key string
	*	@param $func callback
	**/
	function copyfrom($key,$func=NULL) {
		$var=\Base::instance()->get($key);
		if ($func)
			$var=call_user_func($func,$var);
		foreach ($var as $key=>$val)
			$this->document[$key]=$val;
	}

	/**
	*	Populate hive array variable with mapper fields
	*	@return NULL
	*	@param $key string
	**/
	function copyto($key) {
		$var=&\Base::instance()->ref($key);
		foreach ($this->document as $key=>$field)
			$var[$key]=$field;
	}

	/**
	*	Return field names
	*	@return array
	**/
	function fields() {
		return array_keys($this->document);
	}

	/**
	*	Instantiate class
	*	@return void
	*	@param $db object
	*	@param $file string
	**/
	function __construct(\DB\Jig $db,$file) {
		$this->db=$db;
		$this->file=$file;
		$this->reset();
	}

}
