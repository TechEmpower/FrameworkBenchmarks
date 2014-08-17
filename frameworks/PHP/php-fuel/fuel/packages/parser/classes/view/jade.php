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

use Everzet\Jade;

class View_Jade extends \View
{

	protected static $_jade;
	protected static $_cache;

	protected function process_file($file_override = false)
	{
		$file = $file_override ?: $this->file_name;
		static::cache_init($file);

		$file = static::parser()->cache($file);
		return parent::process_file($file);
	}

	public $extension = 'jade';

	/**
	 * Returns the Parser lib object
	 *
	 * @return  Jade\Parser
	 */
	public static function parser()
	{
		if ( ! empty(static::$_parser))
		{
			return static::$_parser;
		}

		$parser = new Jade\Parser(new Jade\Lexer\Lexer());
		$dumper = new Jade\Dumper\PHPDumper();
		$dumper->registerVisitor('tag', new Jade\Visitor\AutotagsVisitor());
		$dumper->registerFilter('javascript', new Jade\Filter\JavaScriptFilter());
		$dumper->registerFilter('cdata', new Jade\Filter\CDATAFilter());
		$dumper->registerFilter('php', new Jade\Filter\PHPFilter());
		$dumper->registerFilter('style', new Jade\Filter\CSSFilter());

		static::$_jade = new Jade\Jade($parser, $dumper, static::$_cache);

		return static::$_jade;
	}

	// Jade stores cached templates as the filename in plain text,
	// so there is a high chance of name collisions (ex: index.jade).
	// This function attempts to create a unique directory for each
	// compiled template.
	public function cache_init($file_path)
	{
		$cache_key = md5($file_path);
		$cache_path = \Config::get('parser.View_Jade.cache_dir', null)
			.substr($cache_key, 0, 2).DS.substr($cache_key, 2, 2);

		if ($cache_path !== null AND ! is_dir($cache_path))
		{
			mkdir($cache_path, 0777, true);
		}

		static::$_cache = $cache_path;
	}

}

/* end of file jade.php */
