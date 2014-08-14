<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_304 extends HTTP_Exception_Expected {

	/**
	 * @var   integer    HTTP 304 Not Modified
	 */
	protected $_code = 304;
	
}