<?php

namespace lithium\tests\mocks\template\view\adapters;

use lithium\util\String;

class TestRenderer extends \lithium\template\view\adapter\File implements \ArrayAccess {
	public static $templateData = array();
	public static $renderData = array();

	public function template($type, array $params) {
		foreach ((array) $this->_paths[$type] as $path) {
			if (!file_exists($path = String::insert($path, $params))) {
				continue;
			}
			self::$templateData[] = compact('type', 'params') + array(
				'return' => $path
			);
			return $path;
		}
		self::$templateData[] = compact('type', 'params') + array(
				'return' => false
			);
		return false;
	}

	public function render($template, $data = array(), array $options = array()) {
		self::$renderData[] = compact('template', 'data', 'options');
		ob_start();
		include $template;
		return ob_get_clean();
	}
}

?>