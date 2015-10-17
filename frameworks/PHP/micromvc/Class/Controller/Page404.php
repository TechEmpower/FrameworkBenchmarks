<?php
/**
 * 404 Page
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Controller;

class Page404 extends \MyController
{
	public function run()
	{
		$this->show_404();
	}
}
