<?php defined('SYSPATH') OR die('No direct script access.');

/**
 * The group wrapper acts as an interface to all the config directives
 * gathered from across the system.
 *
 * This is the object returned from Kohana_Config::load 
 *
 * Any modifications to configuration items should be done through an instance of this object
 *
 * @package    Kohana
 * @category   Configuration
 * @author     Kohana Team
 * @copyright  (c) 2012 Kohana Team
 * @license    http://kohanaphp.com/license
 */
class Kohana_Config_Group extends ArrayObject {

	/**
	 * Reference the config object that created this group
	 * Used when updating config
	 * @var Kohana_Config
	 */
	protected $_parent_instance = NULL;

	/**
	 * The group this config is for
	 * Used when updating config items
	 * @var string
	 */
	protected $_group_name = '';

	/**
	 * Constructs the group object.  Kohana_Config passes the config group 
	 * and its config items to the object here.
	 *
	 * @param Kohana_Config  $instance "Owning" instance of Kohana_Config
	 * @param string         $group    The group name
	 * @param array          $config   Group's config
	 */
	public function __construct(Kohana_Config $instance, $group, array $config = array())
	{
		$this->_parent_instance = $instance;
		$this->_group_name      = $group;

		parent::__construct($config, ArrayObject::ARRAY_AS_PROPS);
	}

	/**
	 * Return the current group in serialized form.
	 *
	 *     echo $config;
	 *
	 * @return  string
	 */
	public function __toString()
	{
		return serialize($this->getArrayCopy());
	}

	/**
	 * Alias for getArrayCopy()
	 *
	 * @return array Array copy of the group's config
	 */
	public function as_array()
	{
		return $this->getArrayCopy();
	}

	/**
	 * Returns the config group's name
	 *
	 * @return string The group name
	 */
	public function group_name()
	{
		return $this->_group_name;
	}
	
	/**
	 * Get a variable from the configuration or return the default value.
	 *
	 *     $value = $config->get($key);
	 *
	 * @param   string  $key        array key
	 * @param   mixed   $default    default value
	 * @return  mixed
	 */
	public function get($key, $default = NULL)
	{
		return $this->offsetExists($key) ? $this->offsetGet($key) : $default;
	}

	/**
	 * Sets a value in the configuration array.
	 *
	 *     $config->set($key, $new_value);
	 *
	 * @param   string  $key    array key
	 * @param   mixed   $value  array value
	 * @return  $this
	 */
	public function set($key, $value)
	{
		$this->offsetSet($key, $value);

		return $this;
	}

	/**
	 * Overrides ArrayObject::offsetSet()
	 * This method is called when config is changed via 
	 *
	 *     $config->var = 'asd';
	 *
	 *     // OR
	 *
	 *     $config['var'] = 'asd';
	 *
	 * @param string $key   The key of the config item we're changing
	 * @param mixed  $value The new array value
	 */
	public function offsetSet($key, $value)
	{
		$this->_parent_instance->_write_config($this->_group_name, $key, $value);

		return parent::offsetSet($key, $value);
	}
}
