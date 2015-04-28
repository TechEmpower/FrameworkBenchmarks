<?php namespace Mail;
/**
 * Transporter Interface
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
interface Transporter_Interface
{
	/**
	 * Send the mail
	 *
	 * @param CCMail 		$mail	The mail object.
	 * @return void
	 *
	 * @throws Mail\Exception
	 */ 
	public function send( CCMail $mail );
}