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

//! Lightweight password hashing library
class Bcrypt extends Prefab {

	//@{ Error messages
	const
		E_Cost='Invalid cost parameter',
		E_Salt='Invalid salt (must be at least 22 alphanumeric characters)';
	//@}

	/**
	*	Generate bcrypt hash of string
	*	@return string|FALSE
	*	@param $pw string
	*	@param $salt string
	*	@param $cost int
	**/
	function hash($pw,$salt=NULL,$cost=10) {
		if ($cost<4 || $cost>31)
			trigger_error(self::E_Cost);
		$len=22;
		if ($salt) {
			if (!preg_match('/^[[:alnum:]\.\/]{'.$len.',}$/',$salt))
				trigger_error(self::E_Salt);
		}
		else {
			$raw=16;
			$iv='';
			if (extension_loaded('mcrypt'))
				$iv=mcrypt_create_iv($raw,MCRYPT_DEV_URANDOM);
			if (!$iv && extension_loaded('openssl'))
				$iv=openssl_random_pseudo_bytes($raw);
			if (!$iv)
				for ($i=0;$i<$raw;$i++)
					$iv.=chr(mt_rand(0,255));
			$salt=str_replace('+','.',base64_encode($iv));
		}
		$salt=substr($salt,0,$len);
		$hash=crypt($pw,sprintf('$2y$%02d$',$cost).$salt);
		return strlen($hash)>13?$hash:FALSE;
	}

	/**
	*	Verify password against hash using timing attack resistant approach
	*	@return bool
	*	@param $pw string
	*	@param $hash string
	**/
	function verify($pw,$hash) {
		$val=crypt($pw,$hash);
		$len=strlen($val);
		if ($len!=strlen($hash) || $len<14)
			return FALSE;
		$out=0;
		for ($i=0;$i<$len;$i++)
			$out|=(ord($val[$i])^ord($hash[$i]));
		return $out===0;
	}

}
