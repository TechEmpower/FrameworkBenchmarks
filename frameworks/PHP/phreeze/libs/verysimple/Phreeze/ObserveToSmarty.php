<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("IObserver.php");
 
/**
 * ObserverToBrowser is an implementation of IObserver that outputs all
 * messages to the smarty debug console
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2005 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class ObserveToSmarty implements IObserver
{
	private $_smarty = null;
	private $_counter = 0;
	
	public function __construct($smarty)
	{
		$this->_smarty = $smarty;
		$this->_smarty->debugging = true;
	}
	
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
		
		$desc = "";

		switch ($ltype)
		{
			case OBSERVE_DEBUG:
				$desc = "DEBUG";
				break;
			case OBSERVE_QUERY:
				$desc = "QUERY";
				$msg = $desc . " " . $msg;
				break;
			case OBSERVE_FATAL:
				$desc = "FATAL";
				break;
			case OBSERVE_INFO:
				$desc = "INFO";
				break;
			case OBSERVE_WARN:
				$desc = "WARN";
				break;
		}
		
		$this->_smarty->assign(str_pad($this->_counter++,3,"0",STR_PAD_LEFT) . "_" . $desc ,$msg);
	}

}

?>