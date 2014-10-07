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

namespace Email;

class SendmailConnectionException extends \FuelException {}

class SendmailFailedException extends \EmailSendingFailedException {}

class Email_Driver_Sendmail extends \Email_Driver
{

	/**
	 * Initalted all needed for Sendmail mailing.
	 *
	 * @return	bool	success boolean
	 */
	protected function _send()
	{
		// Build the message
		$message = $this->build_message();

		// Open a connection
		$return_path = ($this->config['return_path'] !== false) ? $this->config['return_path'] : $this->config['from']['email'];
		$handle = @popen($this->config['sendmail_path'] . " -oi -f ".$return_path." -t", 'w');

		// No connection?
		if(! is_resource($handle))
		{
			throw new \SendmailConnectionException('Could not open a sendmail connection at: '.$this->config['sendmail_path']);
		}

		// Send the headers
		fputs($handle, $message['header']);

		// Send the body
		fputs($handle, $message['body']);

		if(pclose($handle) === -1)
		{
			throw new \SendmailFailedException('Failed sending email through sendmail.');
		}

		return true;
	}

}
