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

use HamlParser;

class View_Haml extends \View
{

	protected static $_parser;
	protected static $_cache;

	protected function process_file($file_override = false)
	{
		$file = $file_override ?: $this->file_name;

		static::cache_init($file);
		$file = static::parser()->parse($file, static::$_cache);

		return parent::process_file($file);
	}

	public $extension = 'haml';

	/**
	 * Returns the Parser lib object
	 *
	 * @return  HamlParser
	 */
	public static function parser()
	{
		if ( ! empty(static::$_parser))
		{
			return static::$_parser;
		}

		static::$_parser = new HamlParser();

		return static::$_parser;
	}

	// Jade stores cached templates as the filename in plain text,
	// so there is a high chance of name collisions (ex: index.jade).
	// This function attempts to create a unique directory for each
	// compiled template.
	// TODO: Extend Jade's caching class?
	public function cache_init($file_path)
	{
		$cache_key = md5($file_path);
		$cache_path = \Config::get('parser.View_Haml.cache_dir', null)
			.substr($cache_key, 0, 2).DS.substr($cache_key, 2, 2);

		if ($cache_path !== null AND ! is_dir($cache_path))
		{
			mkdir($cache_path, 0777, true);
		}

		static::$_cache = $cache_path;
	}

}

/* end of file haml.php */
