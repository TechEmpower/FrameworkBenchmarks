<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_403 extends HTTP_Exception {

	/**
	 * @var   integer    HTTP 403 Forbidden
	 */
	protected $_code = 403;

}