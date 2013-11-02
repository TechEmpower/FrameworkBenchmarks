<?php
class Amenity extends ActiveRecord\Model
{
	static $table_name = 'amenities';
	static $primary_key = 'amenity_id';

	static $has_many = array(
		'property_amenities'
	);
};
?>
