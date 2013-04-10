<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("IObserver.php");
 
/**
 * ObserverToBrowser is an implementation of IObserver that prints all
 * messages to the browser
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2005 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class ObserveToBrowser implements IObserver
{
	
	public function Observe($obj, $ltype = OBSERVE_INFO)
	{
		if (is_object($obj) || is_array($obj))
		{
			$msg = "<pre>" . print_r($obj, 1) . "</pre>";
		}
		else
		{
			$msg = $obj;
		}
		
		switch ($ltype)
		{
			case OBSERVE_DEBUG:
				print "<div class='debug'>$msg</div>\n";
				break;
			case OBSERVE_QUERY:
				print "<div class='query'>$msg</div>\n";
				break;
			case OBSERVE_FATAL:
				print "<div class='fatal'>$msg</div>\n";
				break;
			case OBSERVE_INFO:
				print "<div class='info'>$msg</div>\n";
				break;
			case OBSERVE_WARN:
				print "<div class='warn'>$msg</div>\n";
				break;
		}
	}

}

?>