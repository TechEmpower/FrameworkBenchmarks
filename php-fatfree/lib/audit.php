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

//! Data validator
class Audit extends Prefab {

	//@{ User agents
	const
		UA_Mobile='android|blackberry|iphone|ipod|palm|windows\s+ce',
		UA_Desktop='bsd|linux|os\s+[x9]|solaris|windows',
		UA_Bot='bot|crawl|slurp|spider';
	//@}

	/**
	*	Return TRUE if string is a valid URL
	*	@return bool
	*	@param $str string
	**/
	function url($str) {
		return is_string(filter_var($str,FILTER_VALIDATE_URL));
	}

	/**
	*	Return TRUE if string is a valid e-mail address;
	*	Check DNS MX records if specified
	*	@return bool
	*	@param $str string
	*	@param $mx boolean
	**/
	function email($str,$mx=TRUE) {
		$hosts=array();
		return is_string(filter_var($str,FILTER_VALIDATE_EMAIL)) &&
			(!$mx || getmxrr(substr($str,strrpos($str,'@')+1),$hosts));
	}

	/**
	*	Return TRUE if string is a valid IPV4 address
	*	@return bool
	*	@param $addr string
	**/
	function ipv4($addr) {
		return (bool)filter_var($addr,FILTER_VALIDATE_IP,FILTER_FLAG_IPV4);
	}

	/**
	*	Return TRUE if string is a valid IPV6 address
	*	@return bool
	*	@param $addr string
	**/
	function ipv6($addr) {
		return (bool)filter_var($addr,FILTER_VALIDATE_IP,FILTER_FLAG_IPV6);
	}

	/**
	*	Return TRUE if IP address is within private range
	*	@return bool
	*	@param $addr string
	**/
	function isprivate($addr) {
		return !(bool)filter_var($addr,FILTER_VALIDATE_IP,
			FILTER_FLAG_IPV4|FILTER_FLAG_IPV6|FILTER_FLAG_NO_PRIV_RANGE);
	}

	/**
	*	Return TRUE if IP address is within reserved range
	*	@return bool
	*	@param $addr string
	**/
	function isreserved($addr) {
		return !(bool)filter_var($addr,FILTER_VALIDATE_IP,
			FILTER_FLAG_IPV4|FILTER_FLAG_IPV6|FILTER_FLAG_NO_RES_RANGE);
	}

	/**
	*	Return TRUE if IP address is neither private nor reserved
	*	@return bool
	*	@param $addr string
	**/
	function ispublic($addr) {
		return (bool)filter_var($addr,FILTER_VALIDATE_IP,
			FILTER_FLAG_IPV4|FILTER_FLAG_IPV6|
			FILTER_FLAG_NO_PRIV_RANGE|FILTER_FLAG_NO_RES_RANGE);
	}

	/**
	*	Return TRUE if user agent is a desktop browser
	*	@return bool
	**/
	function isdesktop() {
		$agent=Base::instance()->get('AGENT');
		return (bool)preg_match('/('.self::UA_Desktop.')/i',$agent) &&
			!$this->ismobile();
	}

	/**
	*	Return TRUE if user agent is a mobile device
	*	@return bool
	**/
	function ismobile() {
		$agent=Base::instance()->get('AGENT');
		return (bool)preg_match('/('.self::UA_Mobile.')/i',$agent);
	}

	/**
	*	Return TRUE if user agent is a Web bot
	*	@return bool
	**/
	function isbot() {
		$agent=Base::instance()->get('AGENT');
		return (bool)preg_match('/('.self::UA_Bot.')/i',$agent);
	}

	/**
	*	Return TRUE if specified ID has a valid (Luhn) Mod-10 check digit
	*	@return bool
	*	@param $id string
	**/
	function mod10($id) {
		if (!ctype_digit($id))
			return FALSE;
		$id=strrev($id);
		$sum=0;
		for ($i=0,$l=strlen($id);$i<$l;$i++)
			$sum+=$id[$i]+$i%2*(($id[$i]>4)*-4+$id[$i]%5);
		return !($sum%10);
	}

	/**
	*	Return credit card type if number is valid
	*	@return string|FALSE
	*	@param $id string
	**/
	function card($id) {
		$id=preg_replace('/[^\d]/','',$id);
		if ($this->mod10($id)) {
			if (preg_match('/^3[47][0-9]{13}$/',$id))
				return 'American Express';
			if (preg_match('/^3(?:0[0-5]|[68][0-9])[0-9]{11}$/',$id))
				return 'Diners Club';
			if (preg_match('/^6(?:011|5[0-9][0-9])[0-9]{12}$/',$id))
				return 'Discover';
			if (preg_match('/^(?:2131|1800|35\d{3})\d{11}$/',$id))
				return 'JCB';
			if (preg_match('/^5[1-5][0-9]{14}$/',$id))
				return 'MasterCard';
			if (preg_match('/^4[0-9]{12}(?:[0-9]{3})?$/',$id))
				return 'Visa';
		}
		return FALSE;
	}

	/**
	*	Return entropy estimate of a password (NIST 800-63)
	*	@return int|float
	*	@param $str string
	**/
	function entropy($str) {
		$len=strlen($str);
		return 4*min($len,1)+($len>1?(2*(min($len,8)-1)):0)+
			($len>8?(1.5*(min($len,20)-8)):0)+($len>20?($len-20):0)+
			6*(bool)(preg_match(
				'/[A-Z].*?[0-9[:punct:]]|[0-9[:punct:]].*?[A-Z]/',$str));
	}

}
