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

//! Unicode string manager
class UTF extends Prefab {

	/**
	*	Get string length
	*	@return int
	*	@param $str string
	**/
	function strlen($str) {
		preg_match_all('/./us',$str,$parts);
		return count($parts[0]);
	}

	/**
	*	Reverse a string
	*	@return string
	*	@param $str string
	**/
	function strrev($str) {
		preg_match_all('/./us',$str,$parts);
		return implode('',array_reverse($parts[0]));
	}

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
	*	Find position of first occurrence of a string
	*	@return int|FALSE
	*	@param $stack string
	*	@param $needle string
	*	@param $ofs int
	*	@param $case bool
	**/
	function strpos($stack,$needle,$ofs=0,$case=FALSE) {
		return preg_match('/^(.{'.$ofs.'}.*?)'.
			preg_quote($needle,'/').'/us'.($case?'i':''),$stack,$match)?
			$this->strlen($match[1]):FALSE;
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
		return $this->strstr($stack,$needle,$before,TRUE);
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
		preg_match('/^(.*?)'.preg_quote($needle,'/').'/us'.($case?'i':''),
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
		if ($start<0)
			$start=$this->strlen($str)+$start;
		if (!$len)
			$len=$this->strlen($str)-$start;
		return preg_match('/^.{'.$start.'}(.{0,'.$len.'})/us',$str,$match)?
			$match[1]:FALSE;
	}

	/**
	*	Count the number of substring occurrences
	*	@return int
	*	@param $stack string
	*	@param $needle string
	**/
	function substr_count($stack,$needle) {
		preg_match_all('/'.preg_quote($needle,'/').'/us',$stack,
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

	/**
	*	Convert code points to Unicode symbols
	*	@return string
	*	@param $str string
	**/
	function translate($str) {
		return html_entity_decode(
			preg_replace('/\\\\u([[:xdigit:]]+)/i','&#x\1;',$str));
	}

	/**
	*	Translate emoji tokens to Unicode font-supported symbols
	*	@return string
	*	@param $str string
	**/
	function emojify($str) {
		$map=array(
			':('=>'\u2639', // frown
			':)'=>'\u263a', // smile
			'<3'=>'\u2665', // heart
			':D'=>'\u1f603', // grin
			'XD'=>'\u1f606', // laugh
			';)'=>'\u1f609', // wink
			':P'=>'\u1f60b', // tongue
			':,'=>'\u1f60f', // think
			':/'=>'\u1f623', // skeptic
			'8O'=>'\u1f632', // oops
		)+Base::instance()->get('EMOJI');
		return $this->translate(str_replace(array_keys($map),
			array_values($map),$str));
	}

}
