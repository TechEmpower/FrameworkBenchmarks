<?php 
/*
 *---------------------------------------------------------------
 * Session configuration
 *---------------------------------------------------------------
 */
return array(
	
	/*
	 * This is the default configuration for the main session
	 */
	'main' => array(
		
		// Choose the driver to store the sessions with.
		// We hopefully ship with:
		//     * array
		//     * cookie
		//     * file
		//     * json
		//     * database
		'driver'	 => 'json',
		
		// The session lifetime. 0 means just during the browser session.
		'lifetime'	=> 0,
		
		// The minimum session lifetime, this one is a bit diffrent 
		// This defines how long a session is valid without any user
		// action.
		'min_lifetime'	=> CCDate::minutes(5),
		
		// Garbage collector
		// The gc deletes old and outdated sessions.
		'gc' => array(
			'enabled' => true,
			
			// The factor means how often should gc be executed.
			// ca. every x request. You should scale this value 
			// with the number of page views you got. When you plaan
			// a massive system you should disable gc and delete old
			// sessions using a background job.
			'factor' => 25,
		),
	),
);
