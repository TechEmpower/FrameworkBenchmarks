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

namespace DB;

//! Simple cursor implementation
abstract class Cursor extends \Magic {

	//@{ Error messages
	const
		E_Field='Undefined field %s';
	//@}

	protected
		//! Query results
		$query=array(),
		//! Current position
		$ptr=0;

	/**
	*	Return records (array of mapper objects) that match criteria
	*	@return array
	*	@param $filter string|array
	*	@param $options array
	**/
	abstract function find($filter=NULL,array $options=NULL);

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
	*	Return TRUE if current cursor position is not mapped to any record
	*	@return bool
	**/
	function dry() {
		return empty($this->query[$this->ptr]);
	}

	/**
	*	Return first record (mapper object) that matches criteria
	*	@return object|FALSE
	*	@param $filter string|array
	*	@param $options array
	*	@param $ttl int
	**/
	function findone($filter=NULL,array $options=NULL,$ttl=0) {
		return ($data=$this->find($filter,$options,$ttl))?$data[0]:FALSE;
	}

	/**
	*	Return array containing subset of records matching criteria,
	*	total number of records in superset, number of subsets available,
	*	and actual subset position
	*	@return array
	*	@param $pos int
	*	@param $size int
	*	@param $filter string|array
	*	@param $options array
	**/
	function paginate($pos=0,$size=10,$filter=NULL,array $options=NULL) {
		$total=$this->count($filter,$options);
		$count=ceil($total/$size);
		$pos=max(0,min($pos,$count-1));
		return array(
			'subset'=>$this->find($filter,
				array_merge(
					$options?:array(),
					array('limit'=>$size,'offset'=>$pos*$size)
				)
			),
			'total'=>$total,
			'count'=>$count,
			'pos'=>$pos<$count?$pos:0
		);
	}

	/**
	*	Map to first record that matches criteria
	*	@return array|FALSE
	*	@param $filter string|array
	*	@param $options array
	**/
	function load($filter=NULL,array $options=NULL) {
		return ($this->query=$this->find($filter,$options)) &&
			$this->skip(0)?$this->query[$this->ptr=0]:FALSE;
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
		$this->ptr=0;
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
