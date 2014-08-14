# Validation Example

This example will create user accounts and demonstrate how to handle model and controller validation. We will create a form, process it, and display any errors to the user. We will be assuming that the Model_User class contains a method called `hash_password` that is used to turn the plaintext passwords into some kind of hash. The implementation of the hashing methods are beyond the scope of this example and should be provided with the Authentication library you decide to use.

## SQL schema

	CREATE TABLE IF NOT EXISTS `members` (
	  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
	  `username` varchar(32) NOT NULL,
	  `password` varchar(100) NOT NULL,
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
					array(array($this, 'username_available')),
				),
				'password' => array(
					array('not_empty'),
				),
			);
		}
		
		public function filters()
		{
			return array(
				'password' => array(
					array(array($this, 'hash_password')),
				),
			);
		}

		public function username_available($username)
		{
			// There are simpler ways to do this, but I will use ORM for the sake of the example
			return ORM::factory('Member', array('username' => $username))->loaded();
		}

		public function hash_password($password)
		{
			// Do something to hash the password
		}
	}

## HTML Form

Please forgive my slightly ugly form. I am trying not to use any modules or unrelated magic. :)

	<form action="<?= URL::site('/members'); ?>" method="post" accept-charset="utf-8">
		<label for="username">Username:</label>
		<input id="username" type="text" name="username" value="<?= Arr::get($values, 'username'); ?>" />
		<label for="username" class="error"><?= Arr::get($errors, 'username'); ?>

		<label for="password">Password:</label>
		<input id="password" type="password" name="password" value="<?= Arr::get($values, 'password'); ?>" />
		<label for="password" class="error"><?= Arr::get($errors, 'password'); ?>

		<label for="password_confirm">Repeat Password:</label>
		<input id="password_confirm" type="password" name="_external[password_confirm]" value="<?= Arr::path($values, '_external.password_confirm'); ?>" />
		<label for="password_confirm" class="error"><?= Arr::path($errors, '_external.password_confirm'); ?>

		<button type="submit">Create</button>
	</form>

## Controller

[!!] Remember that the `password` will be hashed as soon as it is set in the model, for this reason, it is impossible to validate it's length or the fact that it matches the `password_confirm` field. The model should not care about validating the `password_confirm` field, so we add that logic to the controller and simply ask the model to bundle the errors into one tidy array. Read the [filters](filters) section to understand how those work.

	public function action_create()
	{
		$view = View::factory('members/create')
			->set('values', $_POST)
			->bind('errors', $errors);

		if ($_POST)
		{
			$member = ORM::factory('Member')
				// The ORM::values() method is a shortcut to assign many values at once
				->values($_POST, array('username', 'password'));

			$external_values = array(
				// The unhashed password is needed for comparing to the password_confirm field
				'password' => Arr::get($_POST, 'password'),
			// Add all external values
			) + Arr::get($_POST, '_external', array());
			$extra = Validation::factory($external_values)
				->rule('password_confirm', 'matches', array(':validation', ':field', 'password'));

			try
			{
				$member->save($extra);
				// Redirect the user to his page
				$this->request->redirect('members/'.$member->id);
			}
			catch (ORM_Validation_Exception $e)
			{
				$errors = $e->errors('models');
			}
		}

		$this->response->body($view);
	}

## Messages

**application/messages/models/member.php**

	return array(
		'username' => array(
			'not_empty' => 'You must provide a username.',
			'min_length' => 'The username must be at least :param2 characters long.',
			'max_length' => 'The username must be less than :param2 characters long.',
			'username_available' => 'This username is not available.',
		),
		'password' => array(
			'not_empty' => 'You must provide a password.',
		),
	);

**application/messages/models/member/_external.php**

	return array(
		'password_confirm' => array(
			'matches' => 'The password fields did not match.',
		),
	);
