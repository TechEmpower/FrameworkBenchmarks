<?php 
/*
 *---------------------------------------------------------------
 * Auth configuration
 *---------------------------------------------------------------
 */
return array(
	
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
		
		// The User model class
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
		
		// login restoring settings
		'restore' => array(
			
			// the user id key cookie name
			'id_cookie' => 'ccauth-restore-id',
			
			// the user restore token cookie name
			'token_cookie' => 'ccauth-restore-token',
			
			// the restore key lifetime
			'lifetime' => \CCDate::months(1),
		),
	),
);
