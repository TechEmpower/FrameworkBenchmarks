<?php
class PropertyAmenity extends ActiveRecord\Model
{
	static $table_name = 'property_amenities';
	static $primary_key = 'id';

	static $belongs_to = array(
		'amenity',
		'property'
	);
};
?>
