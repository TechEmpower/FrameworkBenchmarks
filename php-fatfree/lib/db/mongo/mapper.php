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

namespace DB\Mongo;

//! MongoDB mapper
class Mapper extends \DB\Cursor {

	protected
		//! MongoDB wrapper
		$db,
		//! Mongo collection
		$collection,
		//! Mongo document
		$document=array();

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
		if (!$fw->get('CACHE') || !$ttl || !($cached=$cache->exists(
			$hash=$fw->hash($fw->stringify(array($fields,$filter,$options))).
				'.mongo',$result)) || $cached[0]+$ttl<microtime(TRUE)) {
			if ($options['group']) {
				$tmp=$this->db->selectcollection(
					$fw->get('HOST').'.'.$fw->get('BASE').'.'.uniqid().'.tmp'
				);
				$tmp->batchinsert(
					$this->collection->group(
						$options['group']['keys'],
						$options['group']['initial'],
						$options['group']['reduce'],
						array(
							'condition'=>array(
								$filter,
								$options['group']['finalize']
							)
						)
					),
					array('safe'=>TRUE)
				);
				$filter=array();
				$collection=$tmp;
			}
			else {
				$filter=$filter?:array();
				$collection=$this->collection;
			}
			$cursor=$collection->find($filter,$fields?:array());
			if ($options['order'])
				$cursor=$cursor->sort($options['order']);
			if ($options['limit'])
				$cursor=$cursor->limit($options['limit']);
			if ($options['offset'])
				$cursor=$cursor->skip($options['offset']);
			if ($options['group'])
				$tmp->drop();
			$result=array();
			while ($cursor->hasnext())
				$result[]=$cursor->getnext();
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
	**/
	function count($filter=NULL) {
		return $this->collection->count($filter);
	}

	/**
	*	Return record at specified offset using criteria of previous
	*	load() call and make it active
	*	@return array
	*	@param $ofs int
	**/
	function skip($ofs=1) {
		$this->document=($out=parent::skip($ofs))?$out->document:array();
		return $out;
	}

	/**
	*	Insert new record
	*	@return array
	**/
	function insert() {
		if (isset($this->document['_id']))
			return $this->update();
		$this->collection->insert($this->document);
		return $this->document;
	}

	/**
	*	Update current record
	*	@return array
	**/
	function update() {
		$this->collection->update(
			array('_id'=>$this->document['_id']),
			$this->document,
			array('upsert'=>TRUE)
		);
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
		$result=$this->collection->
			remove(array('_id'=>$this->document['_id']));
		parent::erase();
		$this->skip(0);
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
	**/
	function copyfrom($key) {
		foreach (\Base::instance()->get($key) as $key=>$val)
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
	*	Instantiate class
	*	@return void
	*	@param $db object
	*	@param $collection string
	**/
	function __construct(\DB\Mongo $db,$collection) {
		$this->db=$db;
		$this->collection=$db->{$collection};
		$this->reset();
	}

}
