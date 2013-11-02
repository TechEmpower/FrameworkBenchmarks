<?php
class Payment extends ActiveRecord\Model
{
	// payment belongs to a person
	static $belongs_to = array(
		array('person'),
		array('order'));
}
?>
