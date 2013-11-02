<?php
class Event extends ActiveRecord\Model
{
	static $belongs_to = array(
		'host',
		'venue'
	);

	static $delegate = array(
		array('state', 'address', 'to' => 'venue'),
		array('name', 'to' => 'host', 'prefix' => 'woot')
	);
};
?>
