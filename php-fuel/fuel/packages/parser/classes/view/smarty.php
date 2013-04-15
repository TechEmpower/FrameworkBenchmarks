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

use Smarty;

class View_Smarty extends \View
{
	protected static $_parser;

	protected function process_file($file_override = false)
	{
		$file = $file_override ?: $this->file_name;

		try
		{
			// Smarty doesn't support method chaining
			$parser = static::parser();
			$parser->assign($this->get_data());
			return $parser->fetch($file);
		}
		catch (\Exception $e)
		{
			// Delete the output buffer & re-throw the exception
			ob_end_clean();
			throw $e;
		}
	}

	public $extension = 'smarty';

	/**
	 * Returns the Parser lib object
	 *
	 * @return  Smarty
	 */
	public static function parser()
	{
		if ( ! empty(static::$_parser))
		{
			return static::$_parser;
		}

		// Parser
		static::$_parser = new Smarty();
		static::$_parser->template_dir      = \Config::get('parser.View_Smarty.environment.template_dir', APPPATH.'views'.DS);
		static::$_parser->compile_dir       = \Config::get('parser.View_Smarty.environment.compile_dir', APPPATH.'tmp'.DS.'Smarty'.DS.'templates_c'.DS);
		static::$_parser->config_dir        = \Config::get('parser.View_Smarty.environment.config_dir', APPPATH.'tmp'.DS.'Smarty'.DS.'configs'.DS);
		static::$_parser->cache_dir         = \Config::get('parser.View_Smarty.environment.cache_dir', APPPATH.'cache'.DS.'Smarty'.DS);
		$plugins_dir 						= \Config::get('parser.View_Smarty.environment.plugins_dir', array());
		static::$_parser->addPluginsDir($plugins_dir);

		static::$_parser->caching           = \Config::get('parser.View_Smarty.environment.caching', false);
		static::$_parser->cache_lifetime    = \Config::get('parser.View_Smarty.environment.cache_lifetime', 0);
		static::$_parser->force_compile     = \Config::get('parser.View_Smarty.environment.force_compile', false);
		static::$_parser->compile_check     = \Config::get('parser.View_Smarty.environment.compile_check', true);
		static::$_parser->debugging         = \Config::get('parser.View_Smarty.environment.debugging', false);

		static::$_parser->left_delimiter    = \Config::get('parser.View_Smarty.delimiters.left', '{');
		static::$_parser->right_delimiter   = \Config::get('parser.View_Smarty.delimiters.right', '}');

		static::$_parser->autoload_filters  = \Config::get('parser.View_Smarty.environment.autoload_filters', array());
		static::$_parser->default_modifiers = \Config::get('parser.View_Smarty.environment.default_modifiers', array());

		return static::$_parser;
	}
}

// end of file smarty.php
