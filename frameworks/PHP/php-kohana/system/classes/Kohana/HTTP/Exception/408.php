<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_408 extends HTTP_Exception {

	/**
	 * @var   integer    HTTP 408 Request Timeout
	 */
	protected $_code = 408;

}