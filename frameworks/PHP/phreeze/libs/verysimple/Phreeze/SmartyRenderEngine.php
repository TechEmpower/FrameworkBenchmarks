<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("smarty/Smarty.class.php");
require_once("verysimple/Phreeze/IRenderEngine.php");

/**
 * SmartyRenderEngine is an implementation of IRenderEngine that uses
 * the Smarty template engine to render views
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2011 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class SmartyRenderEngine implements IRenderEngine
{

	/** @var Smarty */
	public $smarty;

	/**
	 * @param string $templatePath
	 * @param string $compilePath
	 */
	function __construct($templatePath = '',$compilePath = '')
	{
		$this->smarty = new Smarty();

		if ($templatePath) $this->smarty->template_dir = $templatePath;

		if ($compilePath)
		{
			$this->smarty->compile_dir = $compilePath;
			$this->smarty->config_dir = $compilePath;
			$this->smarty->cache_dir = $compilePath;
		}
	}

	/**
	 * @see IRenderEngine::assign()
	 */
	function assign($key, $value)
	{
		return $this->smarty->assign($key,$value);
	}

	/**
	 * @see IRenderEngine::display()
	 */
	function display($template)
	{
		return $this->smarty->display($template);
	}

	/**
	 * @see IRenderEngine::fetch()
	 */
	function fetch($template)
	{
		return $this->smarty->fetch($template);
	}

	/**
	 * @see IRenderEngine::clear()
	 */
	function clear($key)
	{
		$this->smarty->clearAssign($key);
	}

	/**
	 * @see IRenderEngine::clearAll()
	 */
	function clearAll()
	{
		$this->smarty->clearAllAssign();
	}

	/**
	 * @see IRenderEngine::getAll()
	 */
	function getAll()
	{
		return $this->smarty->getTemplateVars();
	}


}

?>