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

use Dwoo;
use Dwoo_Compiler;
use Dwoo_Security_Policy;

class View_Dwoo extends \View
{
	protected static $_parser;
	protected static $_parser_compiler;
	protected static $_parser_security;

	protected function process_file($file_override = false)
	{
		$file = $file_override ?: $this->file_name;
		$data = $this->get_data();

		try
		{
			return static::parser()->get($file, $data);
		}
		catch (\Exception $e)
		{
			// Delete the output buffer & re-throw the exception
			ob_end_clean();
			throw $e;
		}
	}

	public $extension = 'tpl';

	/**
	 * Returns the Parser lib object
	 *
	 * @return  Dwoo
	 */
	public static function parser()
	{
		if ( ! empty(static::$_parser))
		{
			return static::$_parser;
		}

		// Parser
		static::$_parser = new Dwoo();
		static::$_parser->setCacheTime(\Config::get('parser.View_Dwoo.environment.cache_time', 0));
		static::$_parser->setCacheDir(\Config::get('parser.View_Dwoo.environment.cache_dir', APPPATH.'cache'.DS.'dwoo'.DS));
		static::$_parser->setCompileDir(\Config::get('parser.View_Dwoo.environment.compile_dir', APPPATH.'cache'.DS.'dwoo'.DS.'compiled'.DS));

		// Compiler
		static::$_parser_compiler = new Dwoo_Compiler;
		static::$_parser_compiler->setAutoEscape(\Config::get('parser.View_Dwoo.environment.autoescape', false));
		static::$_parser_compiler->setLooseOpeningHandling(\Config::get('parser.View_Dwoo.environment.allow_spaces', false));
		static::$_parser_compiler->setNestedCommentsHandling(\Config::get('parser.View_Dwoo.environment.nested_comments', false));
		static::$_parser_compiler->setDelimiters(
			\Config::get('parser.View_Dwoo.delimiters.left', '{'),
			\Config::get('parser.View_Dwoo.delimiters.right', '}')
		);

		// Security
		static::$_parser_security = new Dwoo_Security_Policy;
		static::$_parser_security->setPhpHandling(\Config::get('parser.View_Dwoo.environment.allow_php_tags', 2));
		static::$_parser_security->allowPhpFunction(\Config::get('parser.View_Dwoo.environment.allow_php_func', array()));

		static::$_parser->setSecurityPolicy(static::$_parser_security);

		return static::$_parser;
	}
}

// end of file dwoo.php
