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

//! MongoDB wrapper
class Mongo {

	//@{
	const
		E_Profiler='MongoDB profiler is disabled';
	//@}

	protected
		//! UUID
		$uuid,
		//! Data source name
		$dsn,
		//! MongoDB object
		$db,
		//! MongoDB log
		$log;

	/**
	*	Return data source name
	*	@return string
	**/
	function dsn() {
		return $this->dsn;
	}

	/**
	*	Return UUID
	*	@return string
	**/
	function uuid() {
		return $this->uuid;
	}

	/**
	*	Return MongoDB profiler results
	*	@return string
	**/
	function log() {
		$cursor=$this->selectcollection('system.profile')->find();
		foreach (iterator_to_array($cursor) as $frame)
			if (!preg_match('/\.system\..+$/',$frame['ns']))
				$this->log.=date('r',$frame['ts']->sec).' ('.
					sprintf('%.1f',$frame['millis']).'ms) '.
					$frame['ns'].' ['.$frame['op'].'] '.
					(empty($frame['query'])?
						'':json_encode($frame['query'])).
					(empty($frame['command'])?
						'':json_encode($frame['command'])).
					PHP_EOL;
		return $this->log;
	}

	/**
	*	Intercept native call to re-enable profiler
	*	@return int
	**/
	function drop() {
		$out=$this->db->drop();
		$this->setprofilinglevel(2);
		return $out;
	}

	/**
	*	Redirect call to MongoDB object
	*	@return mixed
	*	@param $func string
	*	@param $args array
	**/
	function __call($func,array $args) {
		return call_user_func_array(array($this->db,$func),$args);
	}

	/**
	*	Instantiate class
	*	@param $dsn string
	*	@param $dbname string
	*	@param $options array
	**/
	function __construct($dsn,$dbname,array $options=NULL) {
		$this->uuid=\Base::instance()->hash($this->dsn=$dsn);
		$class=class_exists('\MongoClient')?'\MongoClient':'\Mongo';
		$this->db=new \MongoDB(new $class($dsn,$options?:array()),$dbname);
		$this->setprofilinglevel(2);
	}

}
