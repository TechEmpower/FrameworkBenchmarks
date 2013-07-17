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

//! MongoDB wrapper
class Mongo extends \MongoDB {

	//@{
	const
		E_Profiler='MongoDB profiler is disabled';
	//@}

	private
		//! MongoDB log
		$log;

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
		$out=parent::drop();
		$this->setprofilinglevel(2);
		return $out;
	}

	/**
	*	Instantiate class
	*	@param $dsn string
	*	@param $dbname string
	*	@param $options array
	**/
	function __construct($dsn,$dbname,array $options=NULL) {
		$class=class_exists('\MongoClient')?'\MongoClient':'\Mongo';
		parent::__construct(new $class($dsn,$options?:array()),$dbname);
		$this->setprofilinglevel(2);
	}

}
