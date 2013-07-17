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

//! Generic array utilities
class Matrix extends Prefab {

	/**
	*	Retrieve values from a specified column of a multi-dimensional
	*	array variable
	*	@return array
	*	@param $var array
	*	@param $col mixed
	**/
	function pick(array $var,$col) {
		return array_map(
			function($row) use($col) {
				return $row[$col];
			},
			$var
		);
	}

	/**
	*	Rotate a two-dimensional array variable
	*	@return NULL
	*	@param $var array
	**/
	function transpose(array &$var) {
		$out=array();
		foreach ($var as $keyx=>$cols)
			foreach ($cols as $keyy=>$valy)
				$out[$keyy][$keyx]=$valy;
		$var=$out;
	}

	/**
	*	Sort a multi-dimensional array variable on a specified column
	*	@return bool
	*	@param $var array
	*	@param $col mixed
	*	@param $order int
	**/
	function sort(array &$var,$col,$order=SORT_ASC) {
		uasort(
			$var,
			function($val1,$val2) use($col,$order) {
				list($v1,$v2)=array($val1[$col],$val2[$col]);
				$out=is_numeric($v1) && is_numeric($v2)?
					Base::instance()->sign($v1-$v2):strcmp($v1,$v2);
				if ($order==SORT_DESC)
					$out=-$out;
				return $out;
			}
		);
		$var=array_values($var);
	}

	/**
	*	Change the key of a two-dimensional array element
	*	@return NULL
	*	@param $var array
	*	@param $old string
	*	@param $new string
	**/
	function changekey(array &$var,$old,$new) {
		$keys=array_keys($var);
		$vals=array_values($var);
		$keys[array_search($old,$keys)]=$new;
		$var=array_combine($keys,$vals);
	}

	/**
	*	Return month calendar of specified date, with optional setting for
	*	first day of week (0 for Sunday)
	*	@return array
	*	@param $date string
	*	@param $first int
	**/
	function calendar($date='now',$first=0) {
		$parts=getdate(strtotime($date));
		$days=cal_days_in_month(CAL_GREGORIAN,$parts['mon'],$parts['year']);
		$ref=date('w',strtotime(date('Y-m',$parts[0]).'-01'))+(7-$first)%7;
		$out=array();
		for ($i=0;$i<$days;$i++)
			$out[floor(($ref+$i)/7)][($ref+$i)%7]=$i+1;
		return $out;
	}

}
