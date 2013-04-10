<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("IObserver.php");

/**
 * IObservable defines an interface for an object that can have listeners attached 
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
interface IObservable
{
    /**
    * Registers/attaches an IObserver to this object
    *
    * @access     public
	* @param IObserver $observer
    */
	public function AttachObserver($observer);

    /**
    * Fires the Observe event on all registered observers
    *
    * @access     public
    * @param variant $obj the $obj or message that you want to log/listen to, etc.
    * @param int $ltype the type/level
    */
	public function Observe($obj, $ltype = OBSERVE_INFO);
}

?>