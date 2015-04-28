<?php namespace Mail;
/**
 * Array Transporter
 * This transporter is just for testing purposes
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Transporter_Array implements Transporter_Interface
{
	/**
	 * Because this is just an array driver we
	 * simply store all send email in this array.
	 */
	public static $store = array();
	
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
		static::$store[] = $mail->export_data();
	}
}