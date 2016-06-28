<?php
/**
 * View
 *
 * Provides fetching of HTML template files
 *
 * @package		MicroMVC
 * @author		http://github.com/tweetmvc/tweetmvc-app
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class View
{

	public static $directory = NULL;

	public static $ext = '.php';

	private $__view = NULL;

	/**
	 * Returns a new view object for the given view.
	 *
	 * @param string $file the view file to load
	 * @param string $module name (blank for current theme)
	 */
	public function __construct($file)
	{
		$this->__view = $file;
	}


	/**
	 * Set an array of values
	 *
	 * @param array $array of values
	 */
	public function set($array)
	{
		foreach($array as $k => $v)
		{
			$this->$k = $v;
		}
	}


	/**
	 * Return the view's HTML
	 *
	 * @return string
	 */
	public function __toString()
	{
		try {
			ob_start();
			extract((array) $this);
			require static::$directory . $this->__view . static::$ext;
			return ob_get_clean();
		}
		catch(\Exception $e)
		{
			\Micro\Error::exception($e);
			return '';
		}
	}

}

// END
