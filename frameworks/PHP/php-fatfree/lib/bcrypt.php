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

//! Lightweight password hashing library
class Bcrypt extends Prefab {

	//@{ Error messages
	const
		E_CostArg='Invalid cost parameter',
		E_SaltArg='Salt must be at least 22 alphanumeric characters';
	//@}

	//! Default cost
	const
		COST=10;

	/**
	*	Generate bcrypt hash of string
	*	@return string|FALSE
	*	@param $pw string
	*	@param $salt string
	*	@param $cost int
	**/
	function hash($pw,$salt=NULL,$cost=self::COST) {
		if ($cost<4 || $cost>31)
			user_error(self::E_CostArg);
		$len=22;
		if ($salt) {
			if (!preg_match('/^[[:alnum:]\.\/]{'.$len.',}$/',$salt))
				user_error(self::E_SaltArg);
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
	*	Check if password is still strong enough
	*	@return bool
	*	@param $hash string
	*	@param $cost int
	**/
	function needs_rehash($hash,$cost=self::COST) {
		list($pwcost)=sscanf($hash,"$2y$%d$");
		return $pwcost<$cost;
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
