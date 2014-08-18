<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;

use MarkdownExtra_Parser;

/**
 * This is a small wrapper around the MarkdownExtra_Parser class.
 *
 * @package     Fuel
 * @subpackage  Core
 */
class Markdown
{
	/**
	 * @var  MarkdownExtra_Parser  The MD parser instance
	 */
	protected static $parser = null;

	/**
	 * Load Markdown and get it setup.
	 *
	 * @return  void
	 */
	public static function _init()
	{
		if ( ! class_exists('MarkdownExtra_Parser', false))
		{
			include COREPATH.'vendor'.DS.'markdown'.DS.'markdown.php';
		}

		static::$parser = new MarkdownExtra_Parser();
	}

	/**
	 * Runs the given text through the Markdown parser.
	 *
	 * @param   string  Text to parse
	 * @return  string
	 */
	public static function parse($text)
	{
		return static::$parser->transform($text);
	}
}
