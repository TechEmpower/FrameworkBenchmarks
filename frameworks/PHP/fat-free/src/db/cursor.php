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

namespace DB;

//! Simple cursor implementation
abstract class Cursor extends \Magic implements \IteratorAggregate {

	//@{ Error messages
	const
		E_Field='Undefined field %s';
	//@}

	protected
		//! Query results
		$query=array(),
		//! Current position
		$ptr=0,
		//! Event listeners
		$trigger=array();

	/**
	*	Return database type
	*	@return string
	**/
	abstract function dbtype();

	/**
	*	Return field names
	*	@return array
	**/
	abstract function fields();

	/**
	*	Return fields of mapper object as an associative array
	*	@return array
	*	@param $obj object
	**/
	abstract function cast($obj=NULL);

	/**
	*	Return records (array of mapper objects) that match criteria
	*	@return array
	*	@param $filter string|array
	*	@param $options array
	*	@param $ttl int
	**/
	abstract function find($filter=NULL,array $options=NULL,$ttl=0);

	/**
	*	Count records that match criteria
	*	@return int
	*	@param $filter array
	*	@param $ttl int
	**/
	abstract function count($filter=NULL,$ttl=0);

	/**
	*	Insert new record
	*	@return array
	**/
	abstract function insert();

	/**
	*	Update current record
	*	@return array
	**/
	abstract function update();

	/**
	*	Hydrate mapper object using hive array variable
	*	@return NULL
	*	@param $var array|string
	*	@param $func callback
	**/
	abstract function copyfrom($var,$func=NULL);

	/**
	*	Populate hive array variable with mapper fields
	*	@return NULL
	*	@param $key string
	**/
	abstract function copyto($key);

	/**
	*	Get cursor's equivalent external iterator
	*	Causes a fatal error in PHP 5.3.5if uncommented
	*	return ArrayIterator
	**/
	abstract function getiterator();


	/**
	*	Return TRUE if current cursor position is not mapped to any record
	*	@return bool
	**/
	function dry() {
		return empty($this->query[$this->ptr]);
	}

	/**
	*	Return first record (mapper object) that matches criteria
	*	@return \DB\Cursor|FALSE
	*	@param $filter string|array
	*	@param $options array
	*	@param $ttl int
	**/
	function findone($filter=NULL,array $options=NULL,$ttl=0) {
		if (!$options)
			$options=array();
		// Override limit
		$options['limit']=1;
		return ($data=$this->find($filter,$options,$ttl))?$data[0]:FALSE;
	}

	/**
	*	Return array containing subset of records matching criteria,
	*	total number of records in superset, specified limit, number of
	*	subsets available, and actual subset position
	*	@return array
	*	@param $pos int
	*	@param $size int
	*	@param $filter string|array
	*	@param $options array
	*	@param $ttl int
	**/
	function paginate(
		$pos=0,$size=10,$filter=NULL,array $options=NULL,$ttl=0) {
		$total=$this->count($filter,$ttl);
		$count=ceil($total/$size);
		$pos=max(0,min($pos,$count-1));
		return array(
			'subset'=>$this->find($filter,
				array_merge(
					$options?:array(),
					array('limit'=>$size,'offset'=>$pos*$size)
				),
				$ttl
			),
			'total'=>$total,
			'limit'=>$size,
			'count'=>$count,
			'pos'=>$pos<$count?$pos:0
		);
	}

	/**
	*	Map to first record that matches criteria
	*	@return array|FALSE
	*	@param $filter string|array
	*	@param $options array
	*	@param $ttl int
	**/
	function load($filter=NULL,array $options=NULL,$ttl=0) {
		return ($this->query=$this->find($filter,$options,$ttl)) &&
			$this->skip(0)?$this->query[$this->ptr=0]:FALSE;
	}

	/**
	*	Return the count of records loaded
	*	@return int
	**/
	function loaded() {
		return count($this->query);
	}

	/**
	*	Map to first record in cursor
	*	@return mixed
	**/
	function first() {
		return $this->skip(-$this->ptr);
	}

	/**
	*	Map to last record in cursor
	*	@return mixed
	**/
	function last() {
		return $this->skip(($ofs=count($this->query)-$this->ptr)?$ofs-1:0);
	}

	/**
	*	Map to nth record relative to current cursor position
	*	@return mixed
	*	@param $ofs int
	**/
	function skip($ofs=1) {
		$this->ptr+=$ofs;
		return $this->ptr>-1 && $this->ptr<count($this->query)?
			$this->query[$this->ptr]:FALSE;
	}

	/**
	*	Map next record
	*	@return mixed
	**/
	function next() {
		return $this->skip();
	}

	/**
	*	Map previous record
	*	@return mixed
	**/
	function prev() {
		return $this->skip(-1);
	}

	/**
	 * Return whether current iterator position is valid.
	 */
	function valid() {
		return !$this->dry();
	}

	/**
	*	Save mapped record
	*	@return mixed
	**/
	function save() {
		return $this->query?$this->update():$this->insert();
	}

	/**
	*	Delete current record
	*	@return int|bool
	**/
	function erase() {
		$this->query=array_slice($this->query,0,$this->ptr,TRUE)+
			array_slice($this->query,$this->ptr,NULL,TRUE);
		$this->skip(0);
	}

	/**
	*	Define onload trigger
	*	@return callback
	*	@param $func callback
	**/
	function onload($func) {
		return $this->trigger['load']=$func;
	}

	/**
	*	Define beforeinsert trigger
	*	@return callback
	*	@param $func callback
	**/
	function beforeinsert($func) {
		return $this->trigger['beforeinsert']=$func;
	}

	/**
	*	Define afterinsert trigger
	*	@return callback
	*	@param $func callback
	**/
	function afterinsert($func) {
		return $this->trigger['afterinsert']=$func;
	}

	/**
	*	Define oninsert trigger
	*	@return callback
	*	@param $func callback
	**/
	function oninsert($func) {
		return $this->afterinsert($func);
	}

	/**
	*	Define beforeupdate trigger
	*	@return callback
	*	@param $func callback
	**/
	function beforeupdate($func) {
		return $this->trigger['beforeupdate']=$func;
	}

	/**
	*	Define afterupdate trigger
	*	@return callback
	*	@param $func callback
	**/
	function afterupdate($func) {
		return $this->trigger['afterupdate']=$func;
	}

	/**
	*	Define onupdate trigger
	*	@return callback
	*	@param $func callback
	**/
	function onupdate($func) {
		return $this->afterupdate($func);
	}

	/**
	*	Define beforesave trigger
	*	@return callback
	*	@param $func callback
	**/
	function beforesave($func) {
		$this->trigger['beforeinsert']=$func;
		$this->trigger['beforeupdate']=$func;
		return $func;
	}

	/**
	*	Define aftersave trigger
	*	@return callback
	*	@param $func callback
	**/
	function aftersave($func) {
		$this->trigger['afterinsert']=$func;
		$this->trigger['afterupdate']=$func;
		return $func;
	}

	/**
	*	Define onsave trigger
	*	@return callback
	*	@param $func callback
	**/
	function onsave($func) {
		return $this->aftersave($func);
	}

	/**
	*	Define beforeerase trigger
	*	@return callback
	*	@param $func callback
	**/
	function beforeerase($func) {
		return $this->trigger['beforeerase']=$func;
	}

	/**
	*	Define aftererase trigger
	*	@return callback
	*	@param $func callback
	**/
	function aftererase($func) {
		return $this->trigger['aftererase']=$func;
	}

	/**
	*	Define onerase trigger
	*	@return callback
	*	@param $func callback
	**/
	function onerase($func) {
		return $this->aftererase($func);
	}

	/**
	*	Reset cursor
	*	@return NULL
	**/
	function reset() {
		$this->query=array();
		$this->ptr=0;
	}

}
