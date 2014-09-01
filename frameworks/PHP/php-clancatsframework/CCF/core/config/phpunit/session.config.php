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
		//     * database
		'driver'	 => 'array',
		
		'gc' => array(
			'enabled' => true,
			'factor' => 1,
		),
		
	),
	
	/*
	 * Test the file driver
	 */
	'file' => array(
		'driver'	 => 'file',
	),
	
	/*
	 * Test the file driver
	 */
	'array' => array(
		'driver'	 => 'array',
	),
);
