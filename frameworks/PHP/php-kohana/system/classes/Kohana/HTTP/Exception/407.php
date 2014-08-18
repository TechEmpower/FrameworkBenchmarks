<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_407 extends HTTP_Exception {

	/**
	 * @var   integer    HTTP 407 Proxy Authentication Required
	 */
	protected $_code = 407;

}