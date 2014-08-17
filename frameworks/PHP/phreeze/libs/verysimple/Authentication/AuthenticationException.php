<?php
/** @package    verysimple::Authentication */
 
 /**
 * AuthenticationException is thrown as a result of "RequirePermission" failing
 * @package    verysimple::Authentication
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class AuthenticationException extends Exception
{
    // Redefine the exception so message isn't optional
    public function __construct($message, $code = 0) 
	{
        // make sure everything is assigned properly
        parent::__construct($message, $code);
    }

}

?>