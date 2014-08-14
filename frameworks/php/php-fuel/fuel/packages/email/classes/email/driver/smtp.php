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


class SmtpConnectionException extends \FuelException {}

class SmtpCommandFailureException extends \EmailSendingFailedException {}

class SmtpTimeoutException extends \EmailSendingFailedException {}

class SmtpAuthenticationFailedException extends \FuelException {}

class Email_Driver_Smtp extends \Email_Driver
{

	/**
	 * The SMTP connection
	 */
	protected $smtp_connection = 0;

	/**
	 * Initalted all needed for SMTP mailing.
	 *
	 * @return	bool	success boolean
	 */
	protected function _send()
	{
		$message = $this->build_message(true);

		if(empty($this->config['smtp']['host']) or empty($this->config['smtp']['port']))
		{
			throw new \FuelException('Must supply a SMTP host and port, none given.');
		}

		// Use authentication?
		$authenticate = ! empty($this->config['smtp']['username']) and ! empty($this->config['smtp']['password']);

		// Connect
		$this->smtp_connect();

		// Authenticate when needed
		$authenticate and $this->smtp_authenticate();

		// Set from
		$this->smtp_send('MAIL FROM:<'.$this->config['from']['email'].'>', 250);

		foreach(array('to', 'cc', 'bcc') as $list)
		{
			foreach($this->{$list} as $recipient)
			{
				$this->smtp_send('RCPT TO:<'.$recipient['email'].'>', array(250, 251));
			}
		}

		// Prepare for data sending
		$this->smtp_send('DATA', 354);

		$lines = explode($this->config['newline'], $message['header'].$this->config['newline'].preg_replace('/^\./m', '..$1', $message['body']));

		foreach($lines as $line)
		{
			if(substr($line, 0, 1) === '.')
			{
				$line = '.'.$line;
			}

			fputs($this->smtp_connection, $line.$this->config['newline']);
		}

		// Finish the message
		$this->smtp_send('.', 250);

		// Close the connection
		$this->smtp_disconnect();

		return true;
	}

	/**
	 * Connects to the given smtp and says hello to the other server.
	 */
	protected function smtp_connect()
	{
		$this->smtp_connection = @fsockopen(
			$this->config['smtp']['host'],
			$this->config['smtp']['port'],
			$error_number,
			$error_string,
			$this->config['smtp']['timeout']
		);

		if(empty($this->smtp_connection))
		{
			throw new \SmtpConnectionException('Could not connect to SMTP: ('.$error_number.') '.$error_string);
		}

		// Clear the smtp response
		$this->smtp_get_response();

		// Just say hello!
		try
		{
			$this->smtp_send('EHLO'.' '.\Input::server('SERVER_NAME', 'localhost.local'), 250);
		}
		catch(\SmtpCommandFailureException $e)
		{
			// Didn't work? Try HELO
			$this->smtp_send('HELO'.' '.\Input::server('SERVER_NAME', 'localhost.local'), 250);
		}

		try
		{
			$this->smtp_send('HELP', 214);
		}
		catch(\SmtpCommandFailureException $e)
		{
			// Let this pass as some servers don't support this.
		}
	}

	/**
	 * Close SMTP connection
	 */
	protected function smtp_disconnect()
	{
		$this->smtp_send('QUIT', 221);
		fclose($this->smtp_connection);
		$this->smtp_connection = 0;
	}

	/**
	 * Performs authentication with the SMTP host
	 */
	protected function smtp_authenticate()
	{
		// Encode login data
		$username = base64_encode($this->config['smtp']['username']);
		$password = base64_encode($this->config['smtp']['password']);

		try
		{
			// Prepare login
			$this->smtp_send('AUTH LOGIN', 334);

			// Send username
			$this->smtp_send($username, 334);

			// Send password
			$this->smtp_send($password, 235);

		}
		catch(\SmtpCommandFailureException $e)
		{
			throw new \SmtpAuthenticationFailedException('Failed authentication.');
		}

	}

	/**
	 * Sends data to the SMTP host
	 *
	 * @param	 string   $data                the SMTP command
	 * @param	 mixed    $expecting           the expected response
	 * @param    bool     $return_number       set to true to return the status number
	 * @return   mixed                         result or result number, false when expecting is false
	 * @throws   SmtpCommandFailureException   when the command failed an expecting is not set to false.
	 */
	protected function smtp_send($data, $expecting, $return_number = false)
	{
		! is_array($expecting) and $expecting !== false and $expecting = array($expecting);

		stream_set_timeout($this->smtp_connection, $this->config['smtp']['timeout']);
		if ( ! fputs($this->smtp_connection, $data . $this->config['newline']))
		{
			if($expecting === false)
			{
				return false;
			}
			throw new \SmtpCommandFailureException('Failed executing command: '. $data);
		}

		$info = stream_get_meta_data($this->smtp_connection);
		if($info['timed_out'])
		{
			throw new \SmtpTimeoutException('SMTP connection timed out.');
		}

		// Get the reponse
		$response = $this->smtp_get_response();

		// Get the reponse number
		$number = (int) substr(trim($response), 0, 3);

		// Check against expected result
		if($expecting !== false and ! in_array($number, $expecting))
		{
			throw new \SmtpCommandFailureException('Got an unexpected response from host on command: ['.$data.'] expecting: '.join(' or ',$expecting).' received: '.$response);
		}

		if($return_number)
		{
			return $number;
		}

		return $response;
	}

	/**
	 * Get SMTP response
	 *
	 * @return	string	SMTP response
	 */
	protected function smtp_get_response()
	{
		$data = '';

		// set the timeout.
		stream_set_timeout($this->smtp_connection, $this->config['smtp']['timeout']);

		while($str = fgets($this->smtp_connection, 512))
		{
			$info = stream_get_meta_data($this->smtp_connection);
			if($info['timed_out'])
			{
				throw new \SmtpTimeoutException('SMTP connection timed out.');
			}

			$data .= $str;

			if (substr($str, 3, 1) === ' ')
			{
				break;
			}
		}

		return $data;
	}

}
