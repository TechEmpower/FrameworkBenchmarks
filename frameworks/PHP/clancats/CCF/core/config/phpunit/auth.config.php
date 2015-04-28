<?php 
/*
 *---------------------------------------------------------------
 * Auth configuration
 *---------------------------------------------------------------
 */
return array(

	'alias' => 'main',
	
	/*
	 * This is the default configuration for the main session
	 */
	'main' => array(

		// Wich session manager should be used?
		// null = default session manager
		'session_manager' => null,

		// On wich field should the current logged in user
		// id be saved in the session?
		'session_key' => 'user_id',

		// On wich field do we select the user for 
		// the authentification
		'user_key' => 'id',

		// The User model
		'user_model' => "\\Auth\\User",
		
		// the identifiers wich fields should be allowed 
		// to select the user object on validation.
		'identifiers' => array(
			'email'
		),

		// Where to store the active logins
		// how long do they stay active etc.
		'logins' => array(

			// the logins db handlerw
			'handler' => null,

			// the logins db table
			'table' => 'auth_logins',
		),
	),
	
	'diffrent_selector_keys' => array(
		'session_key' => 'user_email',
		'user_key' => 'email',
	),
	
	'diffrent_session_manager' => array(
		'session_manager' => 'array',
	),
	
	'same_session_manager' => array(
		'session_manager' => null,
	),
	
	'diffrent_identifiers' => array(
		'identifiers' => array(
			'id'
		),
	),
	
	'multiple_identifiers' => array(
		'identifiers' => array(
			'id',
			'email'
		),
	),
	
	'other' => array(),
);
