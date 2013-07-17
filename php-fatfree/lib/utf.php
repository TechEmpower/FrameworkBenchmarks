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

//! Unicode string manager
class UTF extends Prefab {

	/**
	*	Find position of first occurrence of a string (case-insensitive)
	*	@return int|FALSE
	*	@param $stack string
	*	@param $needle string
	*	@param $ofs int
	**/
	function stripos($stack,$needle,$ofs=0) {
		return $this->strpos($stack,$needle,$ofs,TRUE);
	}

	/**
	*	Get string length
	*	@return int
	*	@param $str string
	**/
	function strlen($str) {
		preg_match_all('/./u',$str,$parts);
		return count($parts[0]);
	}

	/**
	*	Find position of first occurrence of a string
	*	@return int|FALSE
	*	@param $stack string
	*	@param $needle string
	*	@param $ofs int
	*	@param $case bool
	**/
	function strpos($stack,$needle,$ofs=0,$case=FALSE) {
		preg_match('/^(.*?)'.preg_quote($needle,'/').'/u'.($case?'i':''),
			$this->substr($stack,$ofs),$match);
		return isset($match[1])?$this->strlen($match[1]):FALSE;
	}

	/**
	*	Finds position of last occurrence of a string (case-insensitive)
	*	@return int|FALSE
	*	@param $stack string
	*	@param $needle string
	*	@param $ofs int
	**/
	function strripos($stack,$needle,$ofs=0) {
		return $this->strrpos($stack,$needle,$ofs,TRUE);
	}

	/**
	*	Find position of last occurrence of a string
	*	@return int|FALSE
	*	@param $stack string
	*	@param $needle string
	*	@param $ofs int
	*	@param $case bool
	**/
	function strrpos($stack,$needle,$ofs=0,$case=FALSE) {
		if (!$needle)
			return FALSE;
		$len=$this->strlen($stack);
		for ($ptr=$ofs;$ptr<$len;$ptr+=$this->strlen($match[0])) {
			$sub=$this->substr($stack,$ptr);
			if (!$sub || !preg_match('/^(.*?)'.
				preg_quote($needle,'/').'/u'.($case?'i':''),$sub,$match))
				break;
			$ofs=$ptr+$this->strlen($match[1]);
		}
		return $sub?$ofs:FALSE;
	}

	/**
	*	Returns part of haystack string from the first occurrence of
	*	needle to the end of haystack (case-insensitive)
	*	@return string|FALSE
	*	@param $stack string
	*	@param $needle string
	*	@param $before bool
	**/
	function stristr($stack,$needle,$before=FALSE) {
		return strstr($stack,$needle,$before,TRUE);
	}

	/**
	*	Returns part of haystack string from the first occurrence of
	*	needle to the end of haystack
	*	@return string|FALSE
	*	@param $stack string
	*	@param $needle string
	*	@param $before bool
	*	@param $case bool
	**/
	function strstr($stack,$needle,$before=FALSE,$case=FALSE) {
		if (!$needle)
			return FALSE;
		preg_match('/^(.*?)'.preg_quote($needle,'/').'/u'.($case?'i':''),
			$stack,$match);
		return isset($match[1])?
			($before?
				$match[1]:
				$this->substr($stack,$this->strlen($match[1]))):
			FALSE;
	}

	/**
	*	Return part of a string
	*	@return string|FALSE
	*	@param $str string
	*	@param $start int
	*	@param $len int
	**/
	function substr($str,$start,$len=0) {
		if ($start<0) {
			$len=-$start;
			$start=$this->strlen($str)+$start;
		}
		if (!$len)
			$len=$this->strlen($str)-$start;
		return preg_match('/^.{'.$start.'}(.{0,'.$len.'})/u',$str,$match)?
			$match[1]:FALSE;
	}

	/**
	*	Count the number of substring occurrences
	*	@return int
	*	@param $stack string
	*	@param $needle string
	**/
	function substr_count($stack,$needle) {
		preg_match_all('/'.preg_quote($needle,'/').'/u',$stack,
			$matches,PREG_SET_ORDER);
		return count($matches);
	}

	/**
	*	Strip whitespaces from the beginning of a string
	*	@return string
	*	@param $str string
	**/
	function ltrim($str) {
		return preg_replace('/^[\pZ\pC]+/u','',$str);
	}

	/**
	*	Strip whitespaces from the end of a string
	*	@return string
	*	@param $str string
	**/
	function rtrim($str) {
		return preg_replace('/[\pZ\pC]+$/u','',$str);
	}

	/**
	*	Strip whitespaces from the beginning and end of a string
	*	@return string
	*	@param $str string
	**/
	function trim($str) {
		return preg_replace('/^[\pZ\pC]+|[\pZ\pC]+$/u','',$str);
	}

	/**
	*	Return UTF-8 byte order mark
	*	@return string
	**/
	function bom() {
		return chr(0xef).chr(0xbb).chr(0xbf);
	}

}
