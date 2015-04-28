<?php namespace Dev;
/**
 * Development Mail Controller
 **
 * 
 * @package       Dev
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0
 * @copyright     2010 - 2014 ClanCats GmbH
 *
 */
use \CCIn;
use \CCConfig;
use \CCMail;

class MailController extends \CCViewController 
{	
	/**
	 * Mail index
	 */
	public function action_index() 
	{
		// set our session view
		$this->view = \CCView::create( 'Dev::mail' );
		
		list( $from_email, $from_name ) = CCConfig::create( 'mail' )->from;
		
		$this->view->from_email = $from_email;
		$this->view->from_name = $from_name;
		
		$this->view->config_dump = nl2br( str_replace( array( " ", "\t" ), array( '&nbsp;', '&nbsp;&nbsp;' ), print_r( CCConfig::create('mail')->raw(), true ) ) );
		
		// title
		$this->theme->topic = 'Mail Sandbox';
		
		// send a mail?
		if ( CCIn::method( 'post' ) ) 
		{
			// send mails
			$mail = CCMail::create();
			
			$mail->to( CCIn::post( 'to' ) );
			$mail->from( CCIn::post( 'from' ), CCIn::post( 'from_name' ) );
			
			$mail->subject( CCIn::post( 'subject' ) );
			
			$mail->message( CCIn::post( 'message' ) );
			
			foreach( explode( ',', CCIn::post( 'bcc' ) ) as $address ) 
			{
				$mail->bcc( $address );
			}
			
			$mail->is_plaintext( (bool) CCIn::post( 'plaintext', false ) );
			
			// send		
			$mail->send();
			
			\UI\Alert::add( 'success', 'Mail has been send.' );
		}
	}
}