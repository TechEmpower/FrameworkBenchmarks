<?php namespace Mail;
/**
 * File Transporter
 * The file transporter helps debugging in development
 *
 * The will not be sended but stored as file in the storage.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Transporter_File implements Transporter_Interface
{	
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
		$data = $mail->export_data();
		
		$filename = 'mails/'.date( 'Y-m' ).'/'.date( 'd' ).'/'.date("H-i-s").'-'.\CCStr::clean_url( $data['subject'] ).'.log';
		
		\CCFile::append( \CCStorage::path( $filename ), \CCJson::encode( $data, true ) );
	}
}