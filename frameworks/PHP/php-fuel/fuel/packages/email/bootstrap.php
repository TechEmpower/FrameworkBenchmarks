<?php
/**
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */


Autoloader::add_core_namespace('Email');

Autoloader::add_classes(array(
	/**
	 * Email classes.
	 */
	'Email\\Email'							=> __DIR__.'/classes/email.php',
	'Email\\Email_Driver'					=> __DIR__.'/classes/email/driver.php',
	'Email\\Email_Driver_Mail'				=> __DIR__.'/classes/email/driver/mail.php',
	'Email\\Email_Driver_Smtp'				=> __DIR__.'/classes/email/driver/smtp.php',
	'Email\\Email_Driver_Sendmail'			=> __DIR__.'/classes/email/driver/sendmail.php',
	'Email\\Email_Driver_Noop'				=> __DIR__.'/classes/email/driver/noop.php',

	/**
	 * Email exceptions.
	 */
	'Email\\AttachmentNotFoundException'	=> __DIR__.'/classes/email.php',
	'Email\\InvalidAttachmentsException'	=> __DIR__.'/classes/email.php',
	'Email\\InvalidEmailStringEncoding'		=> __DIR__.'/classes/email.php',
	'Email\\EmailSendingFailedException'	=> __DIR__.'/classes/email.php',
	'Email\\EmailValidationFailedException'	=> __DIR__.'/classes/email.php',

	/**
	 * Smtp exceptions
	 */
	'Email\\SmtpTimeoutException'				=> __DIR__.'/classes/email/driver/smtp.php',
	'Email\\SmtpConnectionException'				=> __DIR__.'/classes/email/driver/smtp.php',
	'Email\\SmtpCommandFailureException'			=> __DIR__.'/classes/email/driver/smtp.php',
	'Email\\SmtpAuthenticationFailedException'		=> __DIR__.'/classes/email/driver/smtp.php',

	/**
	 * Sendmail exceptions
	 */
	'Email\\SendmailFailedException'				=> __DIR__.'/classes/email/driver/sendmail.php',
	'Email\\SendmailConnectionException'			=> __DIR__.'/classes/email/driver/sendmail.php',
));
