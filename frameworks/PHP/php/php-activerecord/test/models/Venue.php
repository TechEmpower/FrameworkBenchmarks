<?php
class Venue extends ActiveRecord\Model
{
	static $use_custom_get_state_getter = false;
	static $use_custom_set_state_setter = false;
	
	
	static $has_many = array(
		'events',
		array('hosts', 'through' => 'events')
	);

	static $has_one;

	static $alias_attribute = array(
		'marquee' => 'name',
		'mycity' => 'city'
	);
	
	public function get_state()
	{
		if (self::$use_custom_get_state_getter)
			return strtolower($this->read_attribute('state'));
		else
			return $this->read_attribute('state');
	}
	
	public function set_state($value)
	{
		if (self::$use_custom_set_state_setter)
			return $this->assign_attribute('state', $value . '#');
		else
			return $this->assign_attribute('state', $value);
	}
	
};
?>
