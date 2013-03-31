<?php
/** @package    verysimple::Phreeze */

require_once("IRenderEngine.php");

define('EXT', '.php');
define('BLADE_EXT', '.blade.php');
define('CRLF', "\r\n");
define('DEFAULT_BUNDLE', 'application');
define('MB_STRING', (int) function_exists('mb_get_info'));
define('DS',DIRECTORY_SEPARATOR);

require_once("laravel/paths.php");
require_once("laravel/helpers.php");
require_once("laravel/blade.php");
require_once("laravel/view.php");
require_once("laravel/event.php");
require_once("laravel/bundle.php");
require_once("laravel/session.php");
require_once("laravel/messages.php");
require_once("laravel/section.php");

/**
 * PHPRenderEngine is an implementation of IRenderEngine
 * that uses PHP as the template language
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2010 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class BladeRenderEngine implements IRenderEngine
{
	/** the file path to the template director */
	static $TEMPLATE_PATH;
	
	static $COMPILE_PATH;

	/** stores the assigned vars */
	public $model = Array();

	/**
	 * @param string $templatePath
	 * @param string $compilePath (not used for this render engine)
	 */
	function __construct($templatePath = '',$compilePath = '')
	{
		self::$TEMPLATE_PATH = $templatePath;
		self::$COMPILE_PATH = $compilePath;

		// blade will look for this path to store compiled templates
		$GLOBALS['laravel_paths']['storage'] = self::$COMPILE_PATH;
		
		// attach a handler to the 'View::loader' event so we can tweak the file paths to fit with Phreeze
		Laravel\Event::listen(Laravel\View::loader, function($bundle,$view)
		{
			return BladeRenderEngine::$TEMPLATE_PATH . $view . '.blade.php';
		});
	}

	/**
	 * @inheritdoc
	 */
	public function assign($key,$value)
	{
		$this->model[$key] = $value;
	}

	/**
	 * @inheritdoc
	 */
	public function display($template)
	{
		//die('template = ' . $template);
		$template = str_replace('.tpl', '', $template); // normalize any old smarty template paths
		echo $this->fetch($template);
	}

	/**
	 * Returns the specified model value
	 */
	public function get($key)
	{
		return $this->model[$key];
	}

	/**
	 * @inheritdoc
	 */
	public function fetch($template)
	{
		$view = Laravel\View::make($template, $this->model);
		
		Laravel\Blade::sharpen();
		
		$responses = Laravel\Event::fire(Laravel\View::engine, array($view));
		
		return $responses[0];
	}

	/**
	 * @see IRenderEngine::clear()
	 */
	function clear($key)
	{
		if (array_key_exists($key,$this->model)) unset($this->model[$key]);
	}

	/**
	 * @see IRenderEngine::clearAll()
	 */
	function clearAll()
	{
		$this->model == array();
	}

	/**
	 * @see IRenderEngine::getAll()
	 */
	function getAll()
	{
		return $this->model;
	}
}

?>