<?php
/** @package    verysimple::Phreeze */

/**
 * IRenderEngine is an interface that can be implemented and passed
 * into Phreeze Controller constructor and will be used to
 * render views.
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2010 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
interface IRenderEngine
{

	function __construct($templatePath='',$compilePath='');

	/**
	 * Assigns a value which will be available to the view
	 * @param string $key
	 * @param variant $value
	 */
	public function assign($key,$value);

	/**
	 * Renders and outputs the given template file to the browser
	 * @param string $template
	 */
	public function display($template);

	/**
	 * Renders and returns the given template file as a string
	 * @param string $template
	 * @return string
	 */
	public function fetch($template);

	/**
	 * Unassign a value
	 * @param string $key
	 */
	public function clear($key);

	/**
	 * Clear all assigned variables
	 */
	public function clearAll();

	/**
	 * Return all assigned variables
	 * @return array
	 */
	public function getAll();
}

?>