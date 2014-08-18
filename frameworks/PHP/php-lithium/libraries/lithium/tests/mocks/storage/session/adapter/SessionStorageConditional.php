<?php

namespace lithium\tests\mocks\storage\session\adapter;

class SessionStorageConditional extends \lithium\storage\session\adapter\Memory {

	public function read($key = null, array $options = array()) {
		return isset($options['fail']) ? null : parent::read($key, $options);
	}

	public function write($key, $value, array $options = array()) {
		return isset($options['fail']) ? null : parent::write($key, $value, $options);
	}
}

?>