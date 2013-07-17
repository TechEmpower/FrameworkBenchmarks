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

//! Custom logger
class Log {

	protected
		//! File name
		$file;

	/**
	*	Write specified text to log file
	*	@return string
	*	@param $text string
	*	@param $format string
	**/
	function write($text,$format='r') {
		$fw=Base::instance();
		$fw->write(
			$this->file,
			date($format).
				(isset($_SERVER['REMOTE_ADDR'])?
					(' ['.$_SERVER['REMOTE_ADDR'].']'):'').' '.
			trim($text).PHP_EOL,
			TRUE
		);
	}

	/**
	*	Erase log
	*	@return NULL
	**/
	function erase() {
		@unlink($this->file);
	}

	/**
	*	Instantiate class
	*	@param $file string
	**/
	function __construct($file) {
		$fw=Base::instance();
		if (!is_dir($dir=$fw->get('LOGS')))
			mkdir($dir,Base::MODE,TRUE);
		$this->file=$dir.$file;
	}

}
