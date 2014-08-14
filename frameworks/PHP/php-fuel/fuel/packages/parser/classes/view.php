<?php
/**
 * Fuel
 *
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Parser;

class View extends \Fuel\Core\View
{

	/**
	 * @var  array  Holds the list of loaded files.
	 */
	protected static $loaded_files = array();

	public static function _init()
	{
		\Config::load('parser', true);

		// Get class name
		$class = \Inflector::denamespace(get_called_class());

		if ($class !== __CLASS__)
		{
			// Include necessary files
			foreach ((array) \Config::get('parser.'.$class.'.include', array()) as $include)
			{
				if ( ! array_key_exists($include, static::$loaded_files))
				{
					require $include;
					static::$loaded_files[$include] = true;
				}
			}
		}
	}

	/**
	 * Forges a new View object based on the extension
	 *
	 * @param   string  $file         view filename
	 * @param   array   $data         view data
	 * @param   bool    $auto_encode  auto encode boolean, null for default
	 * @return  object  a new view instance
	 */
	public static function forge($file = null, $data = null, $auto_encode = null)
	{
		$class = null;

		if ($file !== null)
		{
			$extension = pathinfo($file, PATHINFO_EXTENSION);

			$class = \Config::get('parser.extensions.'.$extension, null);
		}

		if ($class === null)
		{
			$class = get_called_class();
		}

		// Only get rid of the extension if it is not an absolute file path
		if ($file !== null and $file[0] !== '/' and $file[1] !== ':')
		{
			$file = $extension ? preg_replace('/\.'.preg_quote($extension).'$/i', '', $file) : $file;
		}

		// Class can be an array config
		if (is_array($class))
		{
			$class['extension'] and $extension = $class['extension'];
			$class = $class['class'];
		}

		// Include necessary files
		foreach ((array) \Config::get('parser.'.$class.'.include', array()) as $include)
		{
			if ( ! array_key_exists($include, static::$loaded_files))
			{
				require $include;
				static::$loaded_files[$include] = true;
			}
		}

		// Instantiate the Parser class without auto-loading the view file
		if ($auto_encode === null)
		{
			$auto_encode = \Config::get('parser.'.$class.'.auto_encode', null);
		}

		$view = new $class(null, $data, $auto_encode);

		if ($file !== null)
		{
			// Set extension when given
			$extension and $view->extension = $extension;

			// Load the view file
			$view->set_filename($file);
		}

		return $view;
	}
}
