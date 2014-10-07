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

namespace DB\Mongo;

//! MongoDB mapper
class Mapper extends \DB\Cursor {

	protected
		//! MongoDB wrapper
		$db,
		//! Mongo collection
		$collection,
		//! Mongo document
		$document=array(),
		//! Mongo cursor
		$cursor;

	/**
	*	Return database type
	*	@return string
	**/
	function dbtype() {
		return 'Mongo';
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
		return $this->document[$key]=$val;
	}

	/**
	*	Retrieve value of field
	*	@return scalar|FALSE
	*	@param $key string
	**/
	function get($key) {
		if ($this->exists($key))
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
		unset($this->document[$key]);
	}

	/**
	*	Convert array to mapper object
	*	@return object
	*	@param $row array
	**/
	protected function factory($row) {
		$mapper=clone($this);
		$mapper->reset();
		foreach ($row as $key=>$val)
			$mapper->document[$key]=$val;
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
		return $obj->document;
	}

	/**
	*	Build query and execute
	*	@return array
	*	@param $fields string
	*	@param $filter array
	*	@param $options array
	*	@param $ttl int
	**/
	function select($fields=NULL,$filter=NULL,array $options=NULL,$ttl=0) {
		if (!$options)
			$options=array();
		$options+=array(
			'group'=>NULL,
			'order'=>NULL,
			'limit'=>0,
			'offset'=>0
		);
		$fw=\Base::instance();
		$cache=\Cache::instance();
		if (!($cached=$cache->exists($hash=$fw->hash($this->db->dsn().
			$fw->stringify(array($fields,$filter,$options))).'.mongo',
			$result)) || !$ttl || $cached[0]+$ttl<microtime(TRUE)) {
			if ($options['group']) {
				$grp=$this->collection->group(
					$options['group']['keys'],
					$options['group']['initial'],
					$options['group']['reduce'],
					array(
						'condition'=>$filter,
						'finalize'=>$options['group']['finalize']
					)
				);
				$tmp=$this->db->selectcollection(
					$fw->get('HOST').'.'.$fw->get('BASE').'.'.
					uniqid(NULL,TRUE).'.tmp'
				);
				$tmp->batchinsert($grp['retval'],array('safe'=>TRUE));
				$filter=array();
				$collection=$tmp;
			}
			else {
				$filter=$filter?:array();
				$collection=$this->collection;
			}
			$this->cursor=$collection->find($filter,$fields?:array());
			if ($options['order'])
				$this->cursor=$this->cursor->sort($options['order']);
			if ($options['limit'])
				$this->cursor=$this->cursor->limit($options['limit']);
			if ($options['offset'])
				$this->cursor=$this->cursor->skip($options['offset']);
			$result=array();
			while ($this->cursor->hasnext())
				$result[]=$this->cursor->getnext();
			if ($options['group'])
				$tmp->drop();
			if ($fw->get('CACHE') && $ttl)
				// Save to cache backend
				$cache->set($hash,$result,$ttl);
		}
		$out=array();
		foreach ($result as $doc)
			$out[]=$this->factory($doc);
		return $out;
	}

	/**
	*	Return records that match criteria
	*	@return array
	*	@param $filter array
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
		return $this->select(NULL,$filter,$options,$ttl);
	}

	/**
	*	Count records that match criteria
	*	@return int
	*	@param $filter array
	*	@param $ttl int
	**/
	function count($filter=NULL,$ttl=0) {
		$fw=\Base::instance();
		$cache=\Cache::instance();
		if (!($cached=$cache->exists($hash=$fw->hash($fw->stringify(
			array($filter))).'.mongo',$result)) || !$ttl ||
			$cached[0]+$ttl<microtime(TRUE)) {
			$result=$this->collection->count($filter);
			if ($fw->get('CACHE') && $ttl)
				// Save to cache backend
				$cache->set($hash,$result,$ttl);
		}
		return $result;
	}

	/**
	*	Return record at specified offset using criteria of previous
	*	load() call and make it active
	*	@return array
	*	@param $ofs int
	**/
	function skip($ofs=1) {
		$this->document=($out=parent::skip($ofs))?$out->document:array();
		if ($this->document && isset($this->trigger['load']))
			\Base::instance()->call($this->trigger['load'],$this);
		return $out;
	}

	/**
	*	Insert new record
	*	@return array
	**/
	function insert() {
		if (isset($this->document['_id']))
			return $this->update();
		if (isset($this->trigger['beforeinsert']))
			\Base::instance()->call($this->trigger['beforeinsert'],
				array($this,array('_id'=>$this->document['_id'])));
		$this->collection->insert($this->document);
		$pkey=array('_id'=>$this->document['_id']);
		if (isset($this->trigger['afterinsert']))
			\Base::instance()->call($this->trigger['afterinsert'],
				array($this,$pkey));
		$this->load($pkey);
		return $this->document;
	}

	/**
	*	Update current record
	*	@return array
	**/
	function update() {
		$pkey=array('_id'=>$this->document['_id']);
		if (isset($this->trigger['beforeupdate']))
			\Base::instance()->call($this->trigger['beforeupdate'],
				array($this,$pkey));
		$this->collection->update(
			$pkey,$this->document,array('upsert'=>TRUE));
		if (isset($this->trigger['afterupdate']))
			\Base::instance()->call($this->trigger['afterupdate'],
				array($this,$pkey));
		return $this->document;
	}

	/**
	*	Delete current record
	*	@return bool
	*	@param $filter array
	**/
	function erase($filter=NULL) {
		if ($filter)
			return $this->collection->remove($filter);
		$pkey=array('_id'=>$this->document['_id']);
		if (isset($this->trigger['beforeerase']))
			\Base::instance()->call($this->trigger['beforeerase'],
				array($this,$pkey));
		$result=$this->collection->
			remove(array('_id'=>$this->document['_id']));
		parent::erase();
		$this->skip(0);
		if (isset($this->trigger['aftererase']))
			\Base::instance()->call($this->trigger['aftererase'],
				array($this,$pkey));
		return $result;
	}

	/**
	*	Reset cursor
	*	@return NULL
	**/
	function reset() {
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
	*	Return the cursor from last query
	*	@return object|NULL
	**/
	function cursor() {
		return $this->cursor;
	}

	/**
	*	Instantiate class
	*	@return void
	*	@param $db object
	*	@param $collection string
	**/
	function __construct(\DB\Mongo $db,$collection) {
		$this->db=$db;
		$this->collection=$db->selectcollection($collection);
		$this->reset();
	}

}
