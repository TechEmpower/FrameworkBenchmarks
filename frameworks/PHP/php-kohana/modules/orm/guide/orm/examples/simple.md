# Simple Examples

This is a simple example of a single ORM model, that has no relationships, but uses validation on the fields. 

## SQL schema

	CREATE TABLE IF NOT EXISTS `members` (
	  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
	  `username` varchar(32) NOT NULL,
	  `first_name` varchar(32) NOT NULL,
	  `last_name` varchar(32) NOT NULL,
	  `email` varchar(127) DEFAULT NULL,
	  PRIMARY KEY (`id`)
	) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1;

## Model
	
	<?php defined('SYSPATH') or die('No direct access allowed.');

	class Model_Member extends ORM {

		public function rules()
		{
			return array(
				'username' => array(
					array('not_empty'),
					array('min_length', array(':value', 4)),
					array('max_length', array(':value', 32)),
					array('regex', array(':value', '/^[-\pL\pN_.]++$/uD')),
				),
				'first_name' => array(
					array('not_empty'),
					array('min_length', array(':value', 4)),
					array('max_length', array(':value', 32)),
					array('regex', array(':value', '/^[-\pL\pN_.]++$/uD')),
				),
				'last_name' => array(
					array('not_empty'),
					array('min_length', array(':value', 4)),
					array('max_length', array(':value', 32)),
					array('regex', array(':value', '/^[-\pL\pN_.]++$/uD')),
				),
				'email' => array(
					array('not_empty'),
					array('min_length', array(':value', 4)),
					array('max_length', array(':value', 127)),
					array('email'),
				),
			);
		}
	}

[!!] The array returned by `ORM::rules()` will be passed to a [Validation] object and tested when you call `ORM::save()`. 

[!!] Please notice that defining the primary key "id" in the model is not necessary. Also the table name in the database is plural and the model name is singular.

## Controller

	<?php defined('SYSPATH') or die('No direct access allowed.');
	
	class Controller_Member extends Controller_Template {
		
		public function action_index()
		{
			/**
			 * Example 1
			 */
			
			// Create an instance of a model
			$members = ORM::factory('Member');
			
			// Get all members with the first name "Peter" find_all()
			// means we get all records matching the query.
			$members->where('first_name', '=', 'Peter')->find_all();

			// Count records in the $members object
			$members->count_all();
			
			/**
			 * Example 2
			 */
			
			// Create an instance of a model
			$member = ORM::factory('Member');
			
			// Get a member with the user name "bongo" find() means
			// we only want the first record matching the query.
			$member->where('username', '=', 'bongo')->find();
			
			/**
			 * Example 3
			 */
			
			// Create an instance of a model
			$member = ORM::factory('Member');
			
			// Do an INSERT query
			$member->username = 'bongo';
			$member->first_name = 'Peter';
			$member->last_name = 'Smith';
			$member->save();
			
			/**
			 * Example 4
			 */
			
			// Create an instance of a model where the
			// table field "id" is "1"
			$member = ORM::factory('Member', 1);
			
			// Do an UPDATE query
			$member->username = 'bongo';
			$member->first_name = 'Peter';
			$member->last_name = 'Smith';
			$member->save();
		}
	}

[!!] $member will be a PHP object where you can access the values from the query e.g. echo $member->first_name
