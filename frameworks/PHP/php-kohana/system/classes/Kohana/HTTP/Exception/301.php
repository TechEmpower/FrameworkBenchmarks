<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_301 extends HTTP_Exception_Redirect {

	/**
	 * @var   integer    HTTP 301 Moved Permanently
	 */
	protected $_code = 301;

}