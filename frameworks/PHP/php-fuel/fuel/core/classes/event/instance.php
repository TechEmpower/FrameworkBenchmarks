<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;

/**
 * Event Class
 *
 * @package		Fuel
 * @category	Core
 * @author		Eric Barnes
 * @author		Harro "WanWizard" Verton
 */
class Event_Instance
{

	/**
	 * @var	array	An array of listeners
	 */
	protected $_events = array();

	// --------------------------------------------------------------------

	/**
	 * Constructor, sets all initial events.
	 *
	 * @param  array  $events  events array
	 */
	public function __construct(array $events = array())
	{
		foreach($events as $event => $callback)
		{
			$this->register($event, $callback);
		}
	}


	// --------------------------------------------------------------------

	/**
	 * Register
	 *
	 * Registers a Callback for a given event
	 *
	 * @access	public
	 * @param	string	The name of the event
	 * @param	mixed	callback information
	 * @return	void
	 */
	public function register()
	{
		// get any arguments passed
		$callback = func_get_args();

		// if the arguments are valid, register the event
		if (isset($callback[0]) and is_string($callback[0]) and isset($callback[1]) and is_callable($callback[1]))
		{
			// make sure we have an array for this event
			isset($this->_events[$callback[0]]) or $this->_events[$callback[0]] = array();

			// store the callback on the call stack
			if (empty($callback[2]))
			{
				array_unshift($this->_events[$callback[0]], $callback);
			}
			else
			{
				$this->_events[$callback[0]][] = $callback;
			}

			// and report success
			return true;
		}
		else
		{
			// can't register the event
			return false;
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Unregister/remove one or all callbacks from event
	 *
	 * @param   string   $event     event to remove from
	 * @param   mixed    $callback  callback to remove [optional, null for all]
	 * @return  boolean  wether one or all callbacks have been removed
	 */
 	public function unregister($event, $callback = null)
	{
		if (isset($this->_events[$event]))
		{
			if ($callback === true)
			{
				$this->_events = array();
				return true;
			}

			foreach ($this->_events[$event] as $i => $arguments)
			{
				if($callback === $arguments[1])
				{
					unset($this->_events[$event][$i]);
					return true;
				}
			}
		}

		return false;
	}

	// --------------------------------------------------------------------

	/**
	 * Trigger
	 *
	 * Triggers an event and returns the results.  The results can be returned
	 * in the following formats:
	 *
	 * 'array'
	 * 'json'
	 * 'serialized'
	 * 'string'
	 *
	 * @access	public
	 * @param	string	 The name of the event
	 * @param	mixed	 Any data that is to be passed to the listener
	 * @param	string	 The return type
	 * @param   boolean  Wether to fire events ordered LIFO instead of FIFO
	 * @return	mixed	 The return of the listeners, in the return type
	 */
	public function trigger($event, $data = '', $return_type = 'string', $reversed = false)
	{
		$calls = array();

		// check if we have events registered
		if ($this->has_events($event))
		{
			$events = $reversed ? array_reverse($this->_events[$event], true) : $this->_events[$event];

			// process them
			foreach ($events as $arguments)
			{
				// get rid of the event name
				array_shift($arguments);

				// get the callback method
				$callback = array_shift($arguments);

				// call the callback event
				if (is_callable($callback))
				{
					$calls[] = call_user_func($callback, $data, $arguments);
				}
			}
		}

		return $this->_format_return($calls, $return_type);
	}

	// --------------------------------------------------------------------

	/**
	 * Has Listeners
	 *
	 * Checks if the event has listeners
	 *
	 * @access	public
	 * @param	string	The name of the event
	 * @return	bool	Whether the event has listeners
	 */
	public function has_events($event)
	{
		if (isset($this->_events[$event]) and count($this->_events[$event]) > 0)
		{
			return true;
		}
		return false;
	}

	// --------------------------------------------------------------------

	/**
	 * Format Return
	 *
	 * Formats the return in the given type
	 *
	 * @access	protected
	 * @param	array	The array of returns
	 * @param	string	The return type
	 * @return	mixed	The formatted return
	 */
	protected function _format_return(array $calls, $return_type)
	{
		switch ($return_type)
		{
			case 'array':
				return $calls;
				break;
			case 'json':
				return json_encode($calls);
				break;
			case 'none':
				return;
			case 'serialized':
				return serialize($calls);
				break;
			case 'string':
				$str = '';
				foreach ($calls as $call)
				{
					$str .= $call;
				}
				return $str;
				break;
			default:
				return $calls;
				break;
		}

		return false;
	}
}

