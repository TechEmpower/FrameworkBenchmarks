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

namespace DB\Mongo;

//! MongoDB-managed session handler
class Session extends Mapper {

	/**
	*	Open session
	*	@return TRUE
	*	@param $path string
	*	@param $name string
	**/
	function open($path,$name) {
		return TRUE;
	}

	/**
	*	Close session
	*	@return TRUE
	**/
	function close() {
		return TRUE;
	}

	/**
	*	Return session data in serialized format
	*	@return string|FALSE
	*	@param $id string
	**/
	function read($id) {
		$this->load(array('session_id'=>$id));
		return $this->dry()?FALSE:$this->get('data');
	}

	/**
	*	Write session data
	*	@return TRUE
	*	@param $id string
	*	@param $data string
	**/
	function write($id,$data) {
		$fw=\Base::instance();
		$headers=$fw->get('HEADERS');
		$this->load(array('session_id'=>$id));
		$this->set('session_id',$id);
		$this->set('data',$data);
		$this->set('ip',$fw->get('IP'));
		$this->set('agent',
			isset($headers['User-Agent'])?$headers['User-Agent']:'');
		$this->set('stamp',time());
		$this->save();
		return TRUE;
	}

	/**
	*	Destroy session
	*	@return TRUE
	*	@param $id string
	**/
	function destroy($id) {
		$this->erase(array('session_id'=>$id));
		return TRUE;
	}

	/**
	*	Garbage collector
	*	@return TRUE
	*	@param $max int
	**/
	function cleanup($max) {
		$this->erase(array('$where'=>'this.stamp+'.$max.'<'.time()));
		return TRUE;
	}

	/**
	*	Return IP address associated with specified session ID
	*	@return string|FALSE
	*	@param $id string
	**/
	function ip($id=NULL) {
		$this->load(array('session_id'=>$id?:session_id()));
		return $this->dry()?FALSE:$this->get('ip');
	}

	/**
	*	Return Unix timestamp associated with specified session ID
	*	@return string|FALSE
	*	@param $id string
	**/
	function stamp($id=NULL) {
		$this->load(array('session_id'=>$id?:session_id()));
		return $this->dry()?FALSE:$this->get('stamp');
	}

	/**
	*	Return HTTP user agent associated with specified session ID
	*	@return string|FALSE
	*	@param $id string
	**/
	function agent($id=NULL) {
		$this->load(array('session_id'=>$id?:session_id()));
		return $this->dry()?FALSE:$this->get('agent');
	}

	/**
	*	Instantiate class
	*	@param $db object
	*	@param $table string
	**/
	function __construct(\DB\Mongo $db,$table='sessions') {
		parent::__construct($db,$table);
		session_set_save_handler(
			array($this,'open'),
			array($this,'close'),
			array($this,'read'),
			array($this,'write'),
			array($this,'destroy'),
			array($this,'cleanup')
		);
		register_shutdown_function('session_commit');
	}

}
