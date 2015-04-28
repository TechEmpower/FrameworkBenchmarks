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
class Transporter_Sendmail extends Transporter_PHPMailer
{
	/**
	 * Set the driver settings ( smtp / sendmail )
	 *
	 * @param PHPMailer 			$driver
	 * @return void
	 */
	protected function setup_driver( &$driver )
	{
		if ( !is_null( $this->config->path ) )
		{
			$driver->Sendmail = $this->config->path;
		}
	}
}