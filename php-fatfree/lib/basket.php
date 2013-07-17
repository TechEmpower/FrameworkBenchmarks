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

//! Session-based pseudo-mapper
class Basket {

	//@{ Error messages
	const
		E_Field='Undefined field %s';
	//@}

	protected
		//! Session key
		$key,
		//! Current item identifier
		$id,
		//! Current item contents
		$item=array();

	/**
	*	Return TRUE if field is defined
	*	@return bool
	*	@param $key string
	**/
	function exists($key) {
		return array_key_exists($key,$this->item);
	}

	/**
	*	Assign value to field
	*	@return scalar|FALSE
	*	@param $key string
	*	@param $val scalar
	**/
	function set($key,$val) {
		return ($key=='_id')?FALSE:($this->item[$key]=$val);
	}

	/**
	*	Retrieve value of field
	*	@return scalar|FALSE
	*	@param $key string
	**/
	function get($key) {
		if ($key=='_id')
			return $this->id;
		if (array_key_exists($key,$this->item))
			return $this->item[$key];
		user_error(sprintf(self::E_Field,$key));
		return FALSE;
	}

	/**
	*	Delete field
	*	@return NULL
	*	@param $key string
	**/
	function clear($key) {
		unset($this->item[$key]);
	}

	/**
	*	Return item that matches key/value pair
	*	@return object|FALSE
	*	@param $key string
	*	@param $val mixed
	**/
	function find($key,$val) {
		if (isset($_SESSION[$this->key]))
			foreach ($_SESSION[$this->key] as $id=>$item)
				if (array_key_exists($key,$item) && $item[$key]==$val) {
					$obj=clone($this);
					$obj->id=$id;
					$obj->item=$item;
					return $obj;
				}
		return FALSE;
	}

	/**
	*	Map current item to matching key/value pair
	*	@return array
	*	@param $key string
	*	@param $val mixed
	**/
	function load($key,$val) {
		if ($found=$this->find($key,$val)) {
			$this->id=$found->id;
			return $this->item=$found->item;
		}
		$this->reset();
		return array();
	}

	/**
	*	Return TRUE if current item is empty/undefined
	*	@return bool
	**/
	function dry() {
		return !$this->item;
	}

	/**
	*	Return number of items in basket
	*	@return int
	**/
	function count() {
		return isset($_SESSION[$this->key])?count($_SESSION[$this->key]):0;
	}

	/**
	*	Save current item
	*	@return array
	**/
	function save() {
		if (!$this->id)
			$this->id=uniqid();
		$_SESSION[$this->key][$this->id]=$this->item;
		session_commit();
		return $this->item;
	}

	/**
	*	Erase item matching key/value pair
	*	@return bool
	*	@param $key string
	*	@param $val mixed
	**/
	function erase($key,$val) {
		$found=$this->find($key,$val);
		if ($found && $id=$found->id) {
			unset($_SESSION[$this->key][$id]);
			session_commit();
			if ($id==$this->id)
				$this->reset();
			return TRUE;
		}
		return FALSE;
	}

	/**
	*	Reset cursor
	*	@return NULL
	**/
	function reset() {
		$this->id=NULL;
		$this->item=array();
	}

	/**
	*	Empty basket
	*	@return NULL
	**/
	function drop() {
		unset($_SESSION[$this->key]);
		session_commit();
	}

	/**
	*	Hydrate item using hive array variable
	*	@return NULL
	*	@param $key string
	**/
	function copyfrom($key) {
		foreach (\Base::instance()->get($key) as $key=>$val)
			$this->item[$key]=$val;
	}

	/**
	*	Populate hive array variable with item contents
	*	@return NULL
	*	@param $key string
	**/
	function copyto($key) {
		$var=&\Base::instance()->ref($key);
		foreach ($this->item as $key=>$field)
			$var[$key]=$field;
	}

	/**
	*	Check out basket contents
	*	@return array
	**/
	function checkout() {
		if (isset($_SESSION[$this->key])) {
			$out=$_SESSION[$this->key];
			unset($_SESSION[$this->key]);
			return $out;
		}
		return array();
	}

	/**
	*	Instantiate class
	*	@return void
	*	@param $key string
	**/
	function __construct($key='basket') {
		$this->key=$key;
		@session_start();
		Base::instance()->sync('SESSION');
		$this->reset();
	}

}
