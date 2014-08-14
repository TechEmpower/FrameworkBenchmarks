<?php
class Host extends ActiveRecord\Model
{
	static $has_many = array(
		'events',
		array('venues', 'through' => 'events')
	);
}
?>
