<?php namespace Laravel\Auth\Drivers; use Laravel\Hash, Laravel\Config;

class Eloquent extends Driver {

	/**
	 * Get the current user of the application.
	 *
	 * If the user is a guest, null should be returned.
	 *
	 * @param  int|object  $token
	 * @return mixed|null
	 */
	public function retrieve($token)
	{
		// We return an object here either if the passed token is an integer (ID)
		// or if we are passed a model object of the correct type
		if (filter_var($token, FILTER_VALIDATE_INT) !== false)
		{
			return $this->model()->find($token);
		}
		else if (is_object($token) and get_class($token) == Config::get('auth.model'))
		{
			return $token;
		}
	}

	/**
	 * Attempt to log a user into the application.
	 *
	 * @param  array $arguments
	 * @return void
	 */
	public function attempt($arguments = array())
	{
		$user = $this->model()->where(function($query) use($arguments)
		{
			$username = Config::get('auth.username');
			
			$query->where($username, '=', $arguments['username']);

			foreach(array_except($arguments, array('username', 'password', 'remember')) as $column => $val)
			{
			    $query->where($column, '=', $val);
			}
		})->first();

		// If the credentials match what is in the database we will just
		// log the user into the application and remember them if asked.
		$password = $arguments['password'];

		$password_field = Config::get('auth.password', 'password');

		if ( ! is_null($user) and Hash::check($password, $user->{$password_field}))
		{
			return $this->login($user->get_key(), array_get($arguments, 'remember'));
		}

		return false;
	}

	/**
	 * Get a fresh model instance.
	 *
	 * @return Eloquent
	 */
	protected function model()
	{
		$model = Config::get('auth.model');

		return new $model;
	}

}
