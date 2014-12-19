<?php namespace Mail;
/**
 * PHPMailer Transporter
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Transporter_PHPMailer implements Transporter_Interface
{
	/**
	 * The Driver configuration
	 *
	 * @var CCConfig
	 */
	public $config; 
	
	/**
	 * Recive and store the transporter configuration
	 */
	public function __construct( $config )
	{
		$this->config = $config;
	}
	
	/**
	 * Send the mail
	 *
	 * @param CCMail 		$mail	The mail object.
	 * @return void
	 *
	 * @throws Mail\Exception
	 */ 
	public function send( CCMail $mail )
	{
		// create new phpmailer instance
		$driver = new PHPMailer\PHPMailer();
		
		// set the charset
		$driver->CharSet = 'utf-8'; 
		
		// set the xmailer tag
		$driver->XMailer = "ClanCatsFramework 2.0 Mail / PHPMailer ".$driver->Version;
		
		// get the mail data as array
		$mail_data = $mail->export_data();
		
		// from address and name
		list( $from_email, $from_name ) = $mail_data['from'];
		
		$driver->From = $from_email;
		if ( !is_null( $from_name ) )
		{
			$driver->FromName = $from_name;
		}
		
		// set the recipients
		foreach( $mail_data['to'] as $email => $name )
		{
			$driver->AddAddress( $email, $name );
		}
		
		// set bcc
		foreach( $mail_data['bcc'] as $email => $name )
		{
			$driver->addBCC( $email, $name );
		}
		
		// set cc
		foreach( $mail_data['cc'] as $email => $name )
		{
			$driver->addCC( $email, $name );
		}
		
		// add attachments
		foreach( $mail_data['attachments'] as $path => $name )
		{
			$driver->addAttachment( $path, $name );
		}
		
		// set the subject
		$driver->Subject = $mail_data['subject'];
		
		// set the message
		$driver->Body = $mail_data['message'];
		$driver->AltBody = $mail_data['plaintext'];
		$driver->IsHTML( !$mail_data['is_plaintext'] );
		
		// setup the driver
		$this->setup_driver( $driver );
		
		if( !$driver->Send() ) 
		{
			throw new Exception( "Mail failure: ". $driver->ErrorInfo );
		}
	}
	
	/**
	 * Set the driver settings ( smtp / sendmail )
	 *
	 * @param PHPMailer 			$driver
	 * @return void
	 */
	protected function setup_driver( &$driver ) {}
}