<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
define("OBSERVE_DEBUG",1);
define("OBSERVE_QUERY",2);
define("OBSERVE_INFO",4);
define("OBSERVE_WARN",8);
define("OBSERVE_FATAL",16);

/**
 * IObserver is an interface that defines an object that can Observe IObservable objects
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
interface IObserver
{
	public function Observe($obj, $ltype = OBSERVE_INFO);
}

?>