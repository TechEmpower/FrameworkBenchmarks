<?php
/*
 *---------------------------------------------------------------
 * Mail configuration
 *---------------------------------------------------------------
 */
return array(
	
	/*
	 * Define the transporters
 	 */
	'transporter' => array(
		
		// This is the default mail transporter
		'main' => array(
			
			// default is diver is unsing sendmail
			'driver' => 'sendmail',
			
			// the sendmail path
			'path' => '/usr/sbin/sendmail',
		),
		
		// This is an example using the smtp diver
		// 'main' => array(
		// 	
		// 	'driver' => 'smtp',
		// 	
		// 	// in this example we use the gmail
		// 	// smtp servers.
		// 	'host'			=> 'smtp.gmail.com',
		//	
		//	// is authentication reqired?
		// 	'auth' 			=> true,
		//	
		//	// your smtp authentication data
		// 	'user'			=> 'example@gmail.com',
		// 	'pass'			=> '<yourpassword>',
		//	
		// 	'encryption'		=> 'ssl',
		// 	'port'			=> 465,
		// ), 
	),
	
	/*
	 * You can disable the entire mailing.
	 * This can be very useful for testing purposes.
	 */
	'disabled' => false,
	
	/*
	 * You can add a layout view, your html mail message will be available
	 * as $content in the layout file.
	 */
	'layout'	 => null,

	/*
	 * catch all emails and send them to another address.
	 */
	'catch_all' => array(
		
		// enable disable this feature
		'enabled' => false,
		
		// to what addresses should the mail be send instead.
		'addresses'	=> array(),
		
		// with what transporter should the mails 
		'transporter' => null,
	),

	/*
	 * every mail will be blind copied to these addresses.
	 */
	'bcc' => array(
	),

	/*
	 * Default from email and name
	 */ 
	'from' => array( 'info@example.com', ClanCats::runtime( 'name' ) ),
);