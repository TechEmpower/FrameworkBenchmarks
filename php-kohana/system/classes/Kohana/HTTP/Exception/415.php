<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_415 extends HTTP_Exception {

	/**
	 * @var   integer    HTTP 415 Unsupported Media Type
	 */
	protected $_code = 415;

}