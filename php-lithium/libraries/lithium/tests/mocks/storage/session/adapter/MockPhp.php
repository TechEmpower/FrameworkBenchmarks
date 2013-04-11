<?php

namespace lithium\tests\mocks\storage\session\adapter;

class MockPhp extends \lithium\storage\session\adapter\Php {

	/**
	 * Overridden method for testing.
	 *
	 * @return boolean false.
	 */
	public static function isStarted() {
		return false;
	}

	/**
	 * Overriden method for testing.
	 *
	 * @return boolean false.
	 */
	protected static function _start() {
		return false;
	}
}

?>