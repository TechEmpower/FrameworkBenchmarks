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

use Mustache;

class View_Mustache extends \View
{
	protected static $_parser;

	protected function process_file($file_override = false)
	{
		$file = $file_override ?: $this->file_name;
		$data = $this->get_data();

		try
		{
			return static::parser()->render(file_get_contents($file), $data);
		}
		catch (\Exception $e)
		{
			// Delete the output buffer & re-throw the exception
			ob_end_clean();
			throw $e;
		}
	}

	public $extension = 'mustache';

	/**
	 * Returns the Parser lib object
	 *
	 * @return  Mustache
	 */
	public static function parser()
	{
		if ( ! empty(static::$_parser))
		{
			return static::$_parser;
		}

		$options = array(
			'delimiters'  => array_values(\Config::get('parser.View_Mustache.delimiters', array('{{','}}'))),
			'charset'     => \Config::get('parser.View_Mustache.environment.charset', 'UTF-8'),
			'pragmas'     => \Config::get('parser.View_Mustache.environment.pragmas', array()),
		);

		static::$_parser = new Mustache(null, null, null, $options);

		return static::$_parser;
	}
}

// end of file mustache.php
