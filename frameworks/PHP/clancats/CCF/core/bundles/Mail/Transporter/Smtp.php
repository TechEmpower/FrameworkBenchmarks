<?php namespace Mail;
/**
 * Sendmail Transporter
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Transporter_Smtp extends Transporter_PHPMailer
{
	/**
	 * Set the driver settings ( smtp / sendmail )
	 *
	 * @param PHPMailer 			$driver
	 * @return void
	 */
	protected function setup_driver( &$driver )
	{
		$driver->IsSMTP(); 
		$driver->Host = $this->config->host;
		
		// smtp auth?
		if ( $this->config->auth === true ) 
		{
			$driver->SMTPAuth = true;
			$driver->Username = $this->config->user;
			$driver->Password = $this->config->pass;
		}
		
		$driver->SMTPSecure = $this->config->encryption;
		$driver->Port = $this->config->port;
	}
}