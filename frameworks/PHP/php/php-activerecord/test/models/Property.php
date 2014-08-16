<?php
class Property extends ActiveRecord\Model
{
	static $table_name = 'property';
	static $primary_key = 'property_id';

	static $has_many = array(
		'property_amenities',
		array('amenities', 'through' => 'property_amenities')
	);
};
?>
