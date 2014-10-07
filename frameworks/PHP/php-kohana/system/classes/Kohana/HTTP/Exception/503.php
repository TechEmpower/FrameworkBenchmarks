<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_503 extends HTTP_Exception {

	/**
	 * @var   integer    HTTP 503 Service Unavailable
	 */
	protected $_code = 503;

}