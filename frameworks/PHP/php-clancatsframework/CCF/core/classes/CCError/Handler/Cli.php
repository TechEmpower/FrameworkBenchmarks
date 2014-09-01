<?php namespace Core;
/**
 * Command line interface error handler
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 * 
 */
class CCError_Handler_Cli extends CCError_Handler
{
	/**
	 * respond information to the user
	 * 
	 * @return void
	 */
	public function respond()
	{
		$inspector = $this->inspector;
		
		CCCli::write( "\n".$inspector->exception_name().': ', 'red' );
		CCCli::line( $inspector->message() );
		CCCli::line( $inspector->exception()->getFile().':'.$inspector->exception()->getLine() );
	}
}