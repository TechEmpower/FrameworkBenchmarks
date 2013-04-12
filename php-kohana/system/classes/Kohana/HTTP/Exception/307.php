<?php defined('SYSPATH') OR die('No direct script access.');

class Kohana_HTTP_Exception_307 extends HTTP_Exception_Redirect {

	/**
	 * @var   integer    HTTP 307 Temporary Redirect
	 */
	protected $_code = 307;

}