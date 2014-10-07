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

//! Legacy mode enabler
class F3 {

	static
		//! Framework instance
		$fw;

	/**
	*	Forward function calls to framework
	*	@return mixed
	*	@param $func callback
	*	@param $args array
	**/
	static function __callstatic($func,array $args) {
		if (!self::$fw)
			self::$fw=Base::instance();
		return call_user_func_array(array(self::$fw,$func),$args);
	}

}
