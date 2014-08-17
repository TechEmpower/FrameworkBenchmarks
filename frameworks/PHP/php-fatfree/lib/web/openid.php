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

namespace Web;

//! OpenID consumer
class OpenID extends \Magic {

	protected
		//! OpenID provider endpoint URL
		$url,
		//! HTTP request parameters
		$args=array();

	/**
	*	Determine OpenID provider
	*	@return string|FALSE
	*	@param $proxy string
	**/
	protected function discover($proxy) {
		// Normalize
		if (!preg_match('/https?:\/\//i',$this->args['identity']))
			$this->args['identity']='http://'.$this->args['identity'];
		$url=parse_url($this->args['identity']);
		// Remove fragment; reconnect parts
		$this->args['identity']=$url['scheme'].'://'.
			(isset($url['user'])?
				($url['user'].
				(isset($url['pass'])?(':'.$url['pass']):'').'@'):'').
			strtolower($url['host']).(isset($url['path'])?$url['path']:'/').
			(isset($url['query'])?('?'.$url['query']):'');
		// HTML-based discovery of OpenID provider
		$req=\Web::instance()->
			request($this->args['identity'],array('proxy'=>$proxy));
		if (!$req)
			return FALSE;
		$type=array_values(preg_grep('/Content-Type:/',$req['headers']));
		if ($type &&
			preg_match('/application\/xrds\+xml|text\/xml/',$type[0]) &&
			($sxml=simplexml_load_string($req['body'])) &&
			($xrds=json_decode(json_encode($sxml),TRUE)) &&
			isset($xrds['XRD'])) {
			// XRDS document
			$svc=$xrds['XRD']['Service'];
			if (isset($svc[0]))
				$svc=$svc[0];
			if (preg_grep('/http:\/\/specs\.openid\.net\/auth\/2.0\/'.
					'(?:server|signon)/',$svc['Type'])) {
				$this->args['provider']=$svc['URI'];
				if (isset($svc['LocalID']))
					$this->args['localidentity']=$svc['LocalID'];
				elseif (isset($svc['CanonicalID']))
					$this->args['localidentity']=$svc['CanonicalID'];
			}
			$this->args['server']=$svc['URI'];
			if (isset($svc['Delegate']))
				$this->args['delegate']=$svc['Delegate'];
		}
		else {
			$len=strlen($req['body']);
			$ptr=0;
			// Parse document
			while ($ptr<$len)
				if (preg_match(
					'/^<link\b((?:\h+\w+\h*=\h*'.
					'(?:"(?:.+?)"|\'(?:.+?)\'))*)\h*\/?>/is',
					substr($req['body'],$ptr),$parts)) {
					if ($parts[1] &&
						// Process attributes
						preg_match_all('/\b(rel|href)\h*=\h*'.
							'(?:"(.+?)"|\'(.+?)\')/s',$parts[1],$attr,
							PREG_SET_ORDER)) {
						$node=array();
						foreach ($attr as $kv)
							$node[$kv[1]]=isset($kv[2])?$kv[2]:$kv[3];
						if (isset($node['rel']) &&
							preg_match('/openid2?\.(\w+)/',
								$node['rel'],$var) &&
							isset($node['href']))
							$this->args[$var[1]]=$node['href'];

					}
					$ptr+=strlen($parts[0]);
				}
				else
					$ptr++;
		}
		// Get OpenID provider's endpoint URL
		if (isset($this->args['provider'])) {
			// OpenID 2.0
			$this->args['ns']='http://specs.openid.net/auth/2.0';
			if (isset($this->args['localidentity']))
				$this->args['identity']=$this->args['localidentity'];
			if (isset($this->args['trust_root']))
				$this->args['realm']=$this->args['trust_root'];
		}
		elseif (isset($this->args['server'])) {
			// OpenID 1.1
			$this->args['ns']='http://openid.net/signon/1.1';
			if (isset($this->args['delegate']))
				$this->args['identity']=$this->args['delegate'];
		}
		if (isset($this->args['provider'])) {
			// OpenID 2.0
			if (empty($this->args['claimed_id']))
				$this->args['claimed_id']=$this->args['identity'];
			return $this->args['provider'];
		}
		elseif (isset($this->args['server']))
			// OpenID 1.1
			return $this->args['server'];
		else
			return FALSE;
	}

	/**
	*	Initiate OpenID authentication sequence; Return FALSE on failure
	*	or redirect to OpenID provider URL
	*	@return bool
	*	@param $proxy string
	*	@param $attr array
	*	@param $reqd string|array
	**/
	function auth($proxy=NULL,$attr=array(),array $reqd=NULL) {
		$fw=\Base::instance();
		$root=$fw->get('SCHEME').'://'.$fw->get('HOST');
		if (empty($this->args['trust_root']))
			$this->args['trust_root']=$root.$fw->get('BASE').'/';
		if (empty($this->args['return_to']))
			$this->args['return_to']=$root.$_SERVER['REQUEST_URI'];
		$this->args['mode']='checkid_setup';
		if ($this->url=$this->discover($proxy)) {
			if ($attr) {
				$this->args['ns.ax']='http://openid.net/srv/ax/1.0';
				$this->args['ax.mode']='fetch_request';
				foreach ($attr as $key=>$val)
					$this->args['ax.type.'.$key]=$val;
				$this->args['ax.required']=is_string($reqd)?
					$reqd:implode(',',$reqd);
			}
			$var=array();
			foreach ($this->args as $key=>$val)
				$var['openid.'.$key]=$val;
			$fw->reroute($this->url.'?'.http_build_query($var));
		}
		return FALSE;
	}

	/**
	*	Return TRUE if OpenID verification was successful
	*	@return bool
	*	@param $proxy string
	**/
	function verified($proxy=NULL) {
		preg_match_all('/(?<=^|&)openid\.([^=]+)=([^&]+)/',
			$_SERVER['QUERY_STRING'],$matches,PREG_SET_ORDER);
		foreach ($matches as $match)
			$this->args[$match[1]]=urldecode($match[2]);
		if (isset($this->args['mode']) &&
			$this->args['mode']!='error' &&
			$this->url=$this->discover($proxy)) {
			$this->args['mode']='check_authentication';
			$var=array();
			foreach ($this->args as $key=>$val)
				$var['openid.'.$key]=$val;
			$req=\Web::instance()->request(
				$this->url,
				array(
					'method'=>'POST',
					'content'=>http_build_query($var),
					'proxy'=>$proxy
				)
			);
			return (bool)preg_match('/is_valid:true/i',$req['body']);
		}
		return FALSE;
	}

	/**
	*	Return OpenID response fields
	*	@return array
	**/
	function response() {
		return $this->args;
	}

	/**
	*	Return TRUE if OpenID request parameter exists
	*	@return bool
	*	@param $key string
	**/
	function exists($key) {
		return isset($this->args[$key]);
	}

	/**
	*	Bind value to OpenID request parameter
	*	@return string
	*	@param $key string
	*	@param $val string
	**/
	function set($key,$val) {
		return $this->args[$key]=$val;
	}

	/**
	*	Return value of OpenID request parameter
	*	@return mixed
	*	@param $key string
	**/
	function get($key) {
		return isset($this->args[$key])?$this->args[$key]:NULL;
	}

	/**
	*	Remove OpenID request parameter
	*	@return NULL
	*	@param $key
	**/
	function clear($key) {
		unset($this->args[$key]);
	}

}

