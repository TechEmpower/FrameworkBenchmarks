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

class View_Phptal extends \View
{

	protected static $_parser;

	protected function process_file($file_override = false)
	{
		$file = $file_override ?: $this->file_name;

		try
		{
			$parser = static::parser();
			foreach($this->get_data() as $key => $value)
			{
				$parser->set($key,$value);
			}
			$parser->setTemplate($file);
			return $parser->execute();
		}
		catch (\Exception $e)
		{
			// Delete the output buffer & re-throw the exception
			ob_end_clean();
			throw $e;
		}
	}

	public $extension = 'phptal';

	public static function parser()
	{
		if ( ! empty(static::$_parser))
		{
			return static::$_parser;
		}

		static::$_parser = new \PHPTAL();
		static::$_parser->setEncoding(\Config::get('parser.View_Phptal.encoding', 'UTF-8'));
		static::$_parser->setOutputMode(constant('\\'.\Config::get('parser.View_Phptal.output_mode', 'PHPTAL::XHTML')));
		static::$_parser->setTemplateRepository(\Config::get('parser.View_Phptal.template_repository', ''));
		static::$_parser->setPhpCodeDestination(\Config::get('parser.View_Phptal.cache_dir', APPPATH.'cache'.DS.'PHPTAL'.DS));
		static::$_parser->setCacheLifetime(\Config::get('parser.View_Phptal.cache_lifetime', 0));
		static::$_parser->setForceReparse(\Config::get('parser.View_Phptal.force_reparse', false));

		return static::$_parser;
	}
}

// end of file phptal.php
