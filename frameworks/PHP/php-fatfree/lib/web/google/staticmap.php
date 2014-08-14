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

namespace Web\Google;

//! Google Static Maps API v2 plug-in
class StaticMap {

	const
		//! API URL
		URL_Static='http://maps.googleapis.com/maps/api/staticmap';

	protected
		//! Query arguments
		$query=array();

	/**
	*	Specify API key-value pair via magic call
	*	@return object
	*	@param $func string
	*	@param $args array
	**/
	function __call($func,array $args) {
		$this->query[]=array($func,$args[0]);
		return $this;
	}

	/**
	*	Generate map
	*	@return string
	**/
	function dump() {
		$fw=\Base::instance();
		$web=\Web::instance();
		$out='';
		return ($req=$web->request(
			self::URL_Static.'?'.array_reduce(
				$this->query,
				function($out,$item) {
					return ($out.=($out?'&':'').
						urlencode($item[0]).'='.urlencode($item[1]));
				}
			))) && $req['body']?$req['body']:FALSE;
	}

}
