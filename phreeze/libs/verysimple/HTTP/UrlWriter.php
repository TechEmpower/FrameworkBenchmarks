<?php
/** @package    verysimple::HTTP */

require_once("verysimple/Phreeze/ActionRouter.php");
require_once("verysimple/HTTP/RequestUtil.php");

/**
 * class for dealing with URLs
 *
 * @package    verysimple::HTTP
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc. http://www.verysimple.com
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class UrlWriter extends ActionRouter
{
	/** Returns a url for the given controller, method and parameters
	 *
	 * @param string $controller
	 * @param string $method
	 * @param string $params in the format param1=val1&param2=val2
	 * @param bool $strip_api set to true to strip virtual part of the url in a rest call
	 * @param string $delim the querystring variable delimiter (& or &amp; for generating valid html)
	 * @return string URL
	 */
	public function Get($controller, $method, $params = "", $strip_api = true, $delim="&")
	{
		$this->stripApi = $strip_api;
		$this->delim = $delim;
		return $this->GetUrl($controller, $method, $params);
	}
}

?>