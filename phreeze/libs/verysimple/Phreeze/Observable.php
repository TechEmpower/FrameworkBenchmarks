<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("IObservable.php");

/**
 * Observable is an abstract implementation of IObservable 
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2005 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
abstract class Observable implements IObservable
{

    private $_observers = Array();
    
    /**
    * Registers an observer with this object
    *
    * @param IObserver $observer
    */
	public function AttachObserver($observer)
	{
		if ($observer)
		{
			$this->_observers[] =& $observer;
		}
	}
	
	/**
	* Fires and observable event.  All registered observers will be notified
	*
	* @param variant $obj a string, numeric or object that contains the observable message
	* @param int $ltype specified the "level" as defined in IObservable
	*/
	public function Observe($obj, $ltype = OBSERVE_INFO)
	{
		foreach ($this->_observers as $observer)
		{
			$observer->Observe($obj, $ltype);
		}
	}
}

?>