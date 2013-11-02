<?php
class AwesomePerson extends ActiveRecord\Model
{
	static $belongs_to = array('author');
}
?>
