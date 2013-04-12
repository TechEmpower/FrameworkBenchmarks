# Fuel Email Package.

A full fledged email class for Fuel. Send mails using php's mail function, sendmail or SMTP.

# Summary

* Send plain/text or html with (optional) alternative plain/text bodies using mail, sendmail or SMTP.
* Add attachments, normal or inline and string or file.
* Automatic inline file attachments for html bodies.
* Configurable attachment paths.

# Usage

	$mail = Email::forge();
	$mail->from('me@domain.com', 'Your Name Here');
	
	// Set to
	$mail->to('mail@domain.com');
	
	// Set with name
	$mail->to('mail@domain.com', 'His/Her Name');
	
	// Set as arrau
	$mail->to(array(
		// Without name
		'mail@domain.com',
		
		// With name
		'mail@domain.com' => 'His/Her Name',
	));
	
	// Work the same for ->cc and ->bcc and ->reply_to
	
	
	// Set a body message
	$email->body('My email body');
	
	// Set a html body message
	$email->html_body(\View::forge('email/template', $email_data));
	
	/**
	
		By default this will also generate an alt body from the html,
		and attach any inline files (not paths like http://...)
	
	**/
	
	// Set an alt body
	$email->alt_body('This is my alt body, for non-html viewers.');
	
	// Set a subject
	$email->subject('This is the subject');
	
	// Change the priority
	$email->priority(\Email::P_HIGH);
	
	// And send it
	$result = $email->send();

# Exceptions

	+ \EmailValidationFailedException, thrown when one or more email addresses doesn't pass validation
	+ \EmailSendingFailedException, thrown when the driver failed to send the exception

Example:

	// Use the default config and change the driver
	$email = \Email::forge('default', array('driver' => 'smtp'));
	$email->subject('My Subject');
	$email->html_body(\View::forge('email/template', $email_data));
	$email->from('me@example.com', 'It's Me!');
	$email->to('other@example.com', 'It's the Other!');
	
	try
	{
		$email->send();
	}
	catch(\EmailValidationFailedException $e)
	{
		// The validation failed
	}
	catch(\EmailSendingFailedException $e)
	{
		// The driver could not send the email
	}
	
# Priorities

These can me one of the following:

	+ \Email::P_LOWEST - 1 (lowest)
	+ \Email::P_LOW - 2 (low)
	+ \Email::P_NORMAL - 3 (normal) - this is the default
	+ \Email::P_HIGH - 4 (high)
	+ \Email::P_HIGHEST - 5 (highest)
	
# Attachments

There are multiple ways to add attachments:

	$email = Email::forge();
	
	// Add an attachment
	$email->attach(DOCROOT.'dir/my_img.png');
	
	// Add an inline attachment
	// Add a cid here to point to the html
	$email->attach(DOCROOT.'dir/my_img.png', true, 'cid:my_conten_id');
	

You can also add string attachments

	$contents = file_get_contents($my_file);
	$email->string_attach($contents, $filename);
	
By default html images are auto included, but it only includes local files.
Look at the following html to see how it works.

	// This is included
	<img src="path/to/my/file.png" />
	
	// This is not included
	<img src="http://remote_host/file.jpeg" />
	
# That's it. Questions? 