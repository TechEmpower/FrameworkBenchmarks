<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_302 extends HTTP_Exception_Redirect {

	/**
	 * @var   integer    HTTP 302 Found
	 */
	protected $_code = 302;

}